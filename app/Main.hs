{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ClassyPrelude
import           Control.Concurrent         (threadDelay, forkIO, ThreadId)
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Class
import qualified Data.Text.IO               as T
import           Network.HTTP.Conduit       (requestHeaders, host)
import           Network.HTTP.Simple        (Response, httpBS, parseRequest,
                                             parseRequest_,
                                             getResponseStatusCode,
                                             getResponseBody, getRequestHeader,
                                             setRequestHeader, Request)
import           Prelude                    ((!!))
import           System.Directory           (getCurrentDirectory)
import           Text.HTML.DOM              (parseLBS)
import           Text.XML                   (Document, Node)
import           Text.XML.Cursor            (attributeIs, Cursor, element,
                                             fromDocument, fromNode,
                                             hasAttribute, node, ($//))

import           DomToHtml
import           Lib

main :: IO ()
main = do
    dir <- getCurrentDirectory
    print dir
    urls <- getURLs "urls.txt"
    --docMay <- runScraping (fmap getDocument (makeConnection "https://bohovibe.co/pages/contact-us") :: Scraping (Maybe Document))
    --forM_ (docMay `mayListBind` getAllContactForms) (putStrLn . nodeToHtml)
    --runScraping (countContactForms urls :: Scraping ())
    --runScraping (getInputsFromURL "https://www.shopolis.com/" :: Scraping [Node]) >>= mapM_ (putStrLn . nodeToHtml)
    runScraping (printInputsFromURLs urls :: Scraping ())

countContactForms :: MonadScraping m => [String] -> m ()
countContactForms bases = do
    results <- forScrapingConc bases (\base -> do
        doc <- searchSiteFormPage base
        let forms = doc `mayListBind` getAllContactForms
        return (pack
            (base ++ " has "
             ++ show (length forms)
             ++ " contact forms.")))
    forM_ results putStrLn

printInputsFromURLs :: MonadScraping m => [String] -> m ()
printInputsFromURLs bases = do
    results <- forScrapingConc bases (\base -> do
        inputs <- getInputsFromURL base
        return ("\n" ++ unlines (pack base : fmap nodeToHtml inputs)))
    forM_ results putStrLn

atomicPrint :: MonadScraping m => Text -> m ()
atomicPrint = atomicScraping envPrintLock . liftIO . putStrLn

getInputsFromURL :: MonadScraping m => String -> m [Node]
getInputsFromURL base = do
    doc <- searchSiteFormPage base
    return ((doc >>= getContactForm) `mayListBind` getSubmittableInputs)

searchSiteFormPage :: MonadScraping m => String -> m (Maybe Document)
searchSiteFormPage base =
    returnFirstSuccessful [relative base path | path <- subpaths] (\url -> do
        response <- makeConnection url
        let doc = getDocument response
        let form = fmap getContactForm doc
        return (form >>= const doc))

mainScraping :: MonadScraping m => [String] -> m ()
mainScraping urls = do
    forScrapingConc urls (\url ->
        tryPaths url subpaths)
    subpathCount <- readScrapingRef envSubpathCount
    print subpathCount

countValidPaths :: MonadScraping m => [String] -> m [(String, [String])]
countValidPaths urls = do
    let paths = [(base, subpaths) | base <- urls] :: [(String, [String])]
    forScrapingConc paths (\(base, subp) -> do
        validPaths <- filterM
            (\p -> do
                let url = relative base p
                response <- makeConnection url
                return (getResponseStatusCode response /= 404))
            subp
        return (base, validPaths))

subpaths :: [String]
subpaths = [ "/pages/contact-us"
           , "/pages/contact"
           , "/contact-us-page/contact-us"
           , "/community/contact"]

userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:65.0) Gecko/20100101 Firefox/65.0"

getURLs :: FilePath -> IO [String]
getURLs path = fmap (fmap unpack . lines) $ T.readFile path

makeRequest url =
    setRequestHeader "User-Agent" [userAgent] (parseRequest_ url)

-- returns the DOM if status is 200
-- otherwise returns Nothing
getDocument :: Response ByteString -> Maybe Document
getDocument response =
    if getResponseStatusCode response == 200
        then Just .
             parseLBS .
             fromStrict .
             getResponseBody $
             response
        else Nothing

getContactForm :: Document -> Maybe Node
getContactForm = headMay . getAllContactForms

getAllContactForms :: Document -> [Node]
getAllContactForms doc =
    fmap node (filter hasTextareaChild (c4))
  where
    getByID id = fromDocument doc $// attributeIs "id" id
    c1 = getByID "contact_form"
    c2 = getByID "contact-form"
    c3 = getByID "contactform"
    c4 = fromDocument doc $// element "form"
    hasTextareaChild :: Cursor -> Bool
    hasTextareaChild c =
        not (null (c $// element "textarea"))

-- from a form gets the inputs and textareas that have a name
getSubmittableInputs :: Node -> [Node]
getSubmittableInputs n =
    fmap node (fromNode n $// hasAttribute "name" >=> inputOrTextarea)
  where
    inputOrTextarea x = element "input" x ++ element "textarea" x

mayListBind :: Maybe a -> (a -> [b]) -> [b]
mayListBind Nothing _ = []
mayListBind (Just x) f = f x

returnFirstSuccessful
    :: Monad m
    => [a]
    -> (a -> m (Maybe b))
    -> m (Maybe b)
returnFirstSuccessful [] _ = return Nothing
returnFirstSuccessful (x:xs) f = do
    yMay <- f x
    maybe
        (returnFirstSuccessful xs f)
        (\y -> return $ Just y)
        yMay

-- Tries a list of paths of a website and returns the first
-- result that doesn't 404
tryPaths :: MonadScraping m
         => String
         -> [String]
         -> m (Maybe (Response ByteString))
tryPaths _ [] = return Nothing
tryPaths base (p:ps) = do
    let url = relative base p
    let mreq = Just $ makeRequest url
    response <- makeConnection url
    if getResponseStatusCode response == 404
        then
            tryPaths base ps
        else do
            recordSuccessfulPath p
            return $ Just response

makeConnection :: MonadScraping m => String -> m (Response ByteString)
makeConnection url  = do
    logTryConnection url
    response <- httpBS $ makeRequest url
    print (url ++ " has status code " ++ show (getResponseStatusCode response))
{--
    if getResponseStatusCode response == 404
        then
            log404Connection url
        else
            logSuccessfullConnection url--}
    return response

logTryConnection h = print ("Attempting to connect to " ++ h)

log404Connection h = print (h ++ " has error 404.")

logSuccessfullConnection h = print $ h ++ " connected"

-- Makes the second URI relative to the first
relative :: String -> String -> String
relative [] r = r
relative [x] r =
    if x=='/' then '/' : rest r else x : '/' : rest r
    where
        rest ('/':xs) = xs
        rest xs = xs
relative (x:xs) r = x : relative xs r

data ScrapingEnv = ScrapingEnv {
    envSubpathCount :: IORef (HashMap String Int),
    envPrintLock :: MVar ()}

newScrapingEnv :: IO ScrapingEnv
newScrapingEnv = do
    subpathCount <- newIORef $ mapFromList []
    printLock <- newMVar ()
    return $ ScrapingEnv
        { envSubpathCount = subpathCount
        , envPrintLock = printLock}

class (MonadReader ScrapingEnv m, MonadIO m) => MonadScraping m where
    runScraping :: m a -> IO a

    scrapingToIO :: m (m a -> IO a)

newtype Scraping a = Scraping
                   { unScraping :: ReaderT ScrapingEnv IO a}
                   deriving ( Functor
                            , Applicative
                            , Monad
                            , MonadReader ScrapingEnv
                            , MonadIO)

instance MonadScraping Scraping where
    runScraping (Scraping s) =
        newScrapingEnv >>= runReaderT s

    scrapingToIO = Scraping
                 { unScraping = do
                       env <- ask
                       return (\(Scraping s) ->
                           runReaderT s env)}

ioEmbedScraping :: MonadScraping m => (IO a -> IO b) -> m a -> m b
ioEmbedScraping f child = do
    convert <- scrapingToIO
    liftIO (f (convert child))

forkScraping :: MonadScraping m => m () -> m ThreadId
forkScraping = ioEmbedScraping forkIO

forScrapingConc :: (Traversable t, MonadScraping m)
                => t a
                -> (a -> m b)
                -> m (t b)
forScrapingConc xs f = do
    convert <- scrapingToIO
    liftIO (forConcurrently xs (convert . f))

recordSuccessfulPath :: MonadScraping m => String -> m ()
recordSuccessfulPath path =
    modifyScrapingRef' envSubpathCount (increment path) 

logMalformedURL :: MonadIO m => String -> m ()
logMalformedURL url = print (url ++ "is a malformed URL")

readScrapingRef :: (MonadReader s m, MonadIO m) => (s -> IORef a) -> m a
readScrapingRef f = reader f >>= liftIO . readIORef

writeScrapingRef :: (MonadReader s m, MonadIO m) => (s -> IORef a) -> a -> m ()
writeScrapingRef f x = do
    ref <- reader f
    liftIO (writeIORef ref x)

modifyScrapingRef' :: (MonadReader s m, MonadIO m)
                   => (s -> IORef a)
                   -> (a -> a)
                   -> m ()
modifyScrapingRef' f g = do
    ref <- reader f
    liftIO $ atomicModifyIORef' ref (\x -> (g x, ()))

adjustWithDefault :: IsMap map
                  => MapValue map
                  -> (MapValue map -> MapValue map)
                  -> ContainerKey map
                  -> map
                  -> map
adjustWithDefault d f x m =
    let y = findWithDefault d x m
        y' = f y
    in
        y' `seq` insertMap x y' m

increment :: (IsMap map, Num (MapValue map))
          => ContainerKey map
          -> map
          -> map
increment = adjustWithDefault 0 (+1)

atomicScraping
    :: MonadScraping m
    => (ScrapingEnv -> MVar ())
    -> m a
    -> m a
atomicScraping f m = do
    lock <- reader f
    convert <- scrapingToIO
    liftIO (withMVar lock (const (convert m)))
