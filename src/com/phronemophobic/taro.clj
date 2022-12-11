(ns com.phronemophobic.taro
  (:require [clojure.java.io :as io])
  (:import [org.apache.commons.compress.archivers.tar
            TarArchiveEntry
            TarArchiveInputStream
            TarArchiveOutputStream]
           [org.apache.commons.compress.archivers.zip
            UnixStat]
           [java.io
            ByteArrayOutputStream
            File
            InputStream
            OutputStream]
           [java.util.zip
            GZIPInputStream
            GZIPOutputStream]))

;; ## Create TAR

(defn- wrap-tar-stream
  ^TarArchiveOutputStream
  [^OutputStream stream]
  (doto (TarArchiveOutputStream. stream)
    (.setLongFileMode TarArchiveOutputStream/LONGFILE_POSIX)))

(defn- add-tar-entry!
  [^TarArchiveOutputStream tar
   {:keys [source ^String path ^int length directory?] :as entry}]
  (let [entry (doto (TarArchiveEntry. path)
                (.setSize length))
        mode (get entry :mode
                  (if directory?
                    0755
                    0644))]
    (.setMode entry (if directory?
                      (bit-or UnixStat/DIR_FLAG mode)
                      (bit-or UnixStat/FILE_FLAG mode)))

    (.putArchiveEntry tar entry)
    (when (not directory?)
      (with-open [in (io/input-stream source)]
        (io/copy in tar :buffer-size 1024)))
    (.closeArchiveEntry tar)))

(defn- relative-path [base file]
  (loop [;; since we're walking up the tree
         ;; use a list to reverse
         parts ()
         file file]
    (if (nil? file)
      (throw (IllegalArgumentException. "file must be child of base."))
      (if (= file base)
        (clojure.string/join File/separatorChar
                             parts)
        (recur (conj parts (.getName file))
               (.getParentFile file))))))

(defn dir->sources [dir]
  (eduction
   (map (fn [^File f]
          (let [path (relative-path dir f)
                path (if (.isDirectory f)
                       (str path File/separatorChar)
                       path)]
            {:source f
             :directory? (.isDirectory f)
             :path (str (.getName dir)
                        File/separatorChar
                        path)
             :length (if (.isDirectory f)
                       0
                       (.length f))})))
   (file-seq dir)))

(defn write-tar!
  "Write entries in dir to out."
  [^OutputStream out dir]
  (with-open [tar (wrap-tar-stream out)]
    (doseq [source (dir->sources dir)]
      (add-tar-entry! tar source))
    (.finish tar)))


(defn write-tar-gz!
  "Write entries in dir to out."
  [^OutputStream out dir]
  (with-open [gz  (GZIPOutputStream. out)
              tar (wrap-tar-stream gz)]
    (doseq [source (dir->sources dir)]
      (add-tar-entry! tar source))
    (.finish tar)))

;; ## Extract TAR

(defn- tar-seq
  [^TarArchiveInputStream tar]
  (when-let [entry (.getNextTarEntry tar)]
    (cons entry (lazy-seq (tar-seq tar)))))

(defn- ensure-parent!
  [^File out]
  (let [parent (.getParentFile out)]
    (when-not (.isDirectory parent)
      (.mkdirs parent))))

(defn- untar*
  ([^InputStream stream write-fn]
   (with-open [tar (TarArchiveInputStream. stream)]
     (doseq [^TarArchiveEntry e (tar-seq tar)
             :when (.isFile e)
             :let [path (.getName e)]]
       (write-fn path tar)))))

(defn untar-seq
  "Create a seq with the contents of the TAR file represented by the given
   input stream. The seq will have the same format as the one expected by
   `tar`."
  [^InputStream stream]
  (let [sources (volatile! [])]
    (->> (fn [path tar]
           (with-open [out (ByteArrayOutputStream.)]
             (io/copy tar out)
             (let [data (.toByteArray out)]
               (vswap! sources conj {:source data
                                     :length (count data)
                                     :path path}))))
         (untar* stream))
    @sources))

(defn untar
  "Untar the given file to the given path. If `file-fn` is given, it will be
   called with `path` and the file's relative path and should produce a
   `java.io.File` object to extract to."
  ([^InputStream stream path]
   (untar stream path io/file))
  ([^InputStream stream path file-fn]
   (let [target (io/file path)]
     (when-not (.isDirectory target)
       (.mkdirs target))
     (->> (fn [file-path tar]
            (let [out (file-fn target file-path)]
              (ensure-parent! out)
              (io/copy tar out)))
          (untar* stream)))))

;; ## Helpers

(defmacro with-gz-open
  "Helper Macro to handle GZip streams, acting like 'with-open' and
   transparently uncompressing the contents."
  [[sym stream] & body]
  `(with-open [stream# ~stream
               ~sym (GZIPInputStream. stream#)]
     ~@body))
