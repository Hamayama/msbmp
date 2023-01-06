;; -*- coding: utf-8 -*-
;;
;; msbmp.scm
;; 2023-1-6 v1.00
;;
;; ＜内容＞
;;   Gauche で、Windows のビットマップファイル (画像ファイル) を、
;;   読み書きするためのモジュールです。
;;   ファイルは、24 bit カラーで無圧縮のもののみ読み書き可能です。
;;
(define-module msbmp
  (use gauche.collection)
  (use binary.pack)
  (export
    <ms-bitmap-data>
    load-ms-bitmap-file
    save-ms-bitmap-file
    ))
(select-module msbmp)

;; 画像データクラス
(define-class <ms-bitmap-data> ()
  ((width  :init-value 0) ; 画像の幅(px)
   (height :init-value 0) ; 画像の高さ(px)
   (data   :init-form (make-u8vector 0))
   ;                      ; 画像データ(u8vector)(1ピクセルは4バイト(RGBA))
   ))

;; Windows のビットマップファイルの読み込み
;;   ・ファイルは、24 bit カラーで無圧縮のもののみ読み込み可能
;;   ・透明色はオプション引数に '(R G B) のリストで指定する(各色は0-255の値)
(define (load-ms-bitmap-file file :optional (trans-color #f))
  (define (err msg . rest)
    (apply errorf (format "bitmap file load error (file=~a)\n~a" file msg) rest))
  (define (get-one-param header index)
    (rlet1 p (~ header index)
      (if (eof-object? p) (err "file size is too small"))))
  (define (read-one-data in)
    (rlet1 d (read-byte in)
      (if (eof-object? d) (err "file size is too small"))))
  (define img (make <ms-bitmap-data>))
  (call-with-input-file file
    (lambda (in)
      ;; ファイルヘッダーの読み込み
      (let* ((file-header (unpack "nVvvV" :input in))
             (ftype       (get-one-param file-header 0))
             (fsize       (get-one-param file-header 1))
             (foffbits    (get-one-param file-header 4))
             (pos         0))
        (unless (= ftype #x424D)
          (err "file type is invalid (ftype=~4,'0Xh)" ftype))
        ;; (fsize はチェックしない)
        ;(unless (>= fsize 0)
        ;  (err "file size is invalid (fsize=~d)" fsize))
        (unless (>= foffbits 0)
          (err "file offset is invalid (foffbits=~d)" foffbits))
        (set! pos (+ pos 14))
        ;; 情報ヘッダーの読み込み
        (let* ((info-header  (unpack "VV!V!vvVVV!V!VV" :input in))
               (isize        (get-one-param info-header 0))
               (iwidth       (get-one-param info-header 1))
               (iheight      (get-one-param info-header 2))
               (ibitcount    (get-one-param info-header 4))
               (icompression (get-one-param info-header 5))
               (isizeimage   (get-one-param info-header 6)))
          ;; (サイズの大きい拡張版が存在する)
          ;(unless (= isize 40)
          (unless (>= isize 40)
            (err "can't load this type of bitmap (isize=~d)" isize))
          (unless (>= iwidth 0)
            (err "can't load this type of bitmap (iwidth=~d)" iwidth))
          (unless (>= iheight 0)
            (err "can't load this type of bitmap (iheight=~d)" iheight))
          (unless (= ibitcount 24)
            (err "can't load this type of bitmap (ibitcount=~d)" ibitcount))
          (unless (= icompression 0)
            (err "can't load this type of bitmap (icompression=~d)" icompression))
          ;; (isizeimage はチェックしない)
          ;(unless (>= isizeimage 0)
          ;  (err "can't load this type of bitmap (isizeimage=~d)" isizeimage))
          (set! pos (+ pos 40))
          ;; 画像データの読み込み
          ;; (上下反転しているので、ここで戻す)
          (let* ((data-size (* iwidth iheight))
                 (data      (make-u8vector (* data-size 4) 0))
                 (trans-r   (list-ref trans-color 0 -1))
                 (trans-g   (list-ref trans-color 1 -1))
                 (trans-b   (list-ref trans-color 2 -1)))
            (do ((i pos i)
                 (j 0   j)
                 (k (* iwidth 4 (- iheight 1)) k)
                 (c 0   c)
                 (x 0   x))
                ;; (fsize と isizeimage はチェックしない)
                ;((or (>= i fsize) (>= j isizeimage) (>= c data-size)) #f)
                ((>= c data-size) #f)
              ;(print i " " fsize " " j " " isizeimage " " c " " data-size)
              (cond
               ;; オフセットの位置まで読み飛ばす
               ((< i foffbits)
                (read-one-data in)
                (inc! i))
               ;; 1ライン読み込み後は、4バイト境界まで読み飛ばす
               ((>= x iwidth)
                (cond
                 ((= (modulo j 4) 0)
                  (set! x 0)
                  (set! k (- k (* iwidth 4 2))))
                 (else
                  (read-one-data in)
                  (inc! i)
                  (inc! j))))
               ;; 1ピクセル分のデータを読み込む
               (else
                (let* ((b (read-one-data in))
                       (g (read-one-data in))
                       (r (read-one-data in))
                       (a (if (and (= r trans-r) (= g trans-g) (= b trans-b)) 0 255)))
                  (set! (~ data k)       r)
                  (set! (~ data (+ k 1)) g)
                  (set! (~ data (+ k 2)) b)
                  (set! (~ data (+ k 3)) a)
                  (set! i (+ i 3))
                  (set! j (+ j 3))
                  (set! k (+ k 4))
                  (inc! c)
                  (inc! x)
                  ))))
            ;; 戻り値をセット
            ;(print iwidth " " iheight " " data)
            (set! (~ img 'width)  iwidth)
            (set! (~ img 'height) iheight)
            (set! (~ img 'data)   data)
            )))
      ))
  img)

;; Windows のビットマップファイルの書き込み
;;   ・ファイルは、24 bit カラーで無圧縮のもののみ書き込み可能
;;   ・透明色の情報は、失われる
(define (save-ms-bitmap-file file img)
  (define (err msg . rest)
    (apply errorf (format "bitmap file save error (file=~a)\n~a" file msg) rest))
  (define (get-one-data data i)
    (~ data i))
  (define (write-one-data d out)
    (write-byte d out))
  (call-with-output-file file
    (lambda (out)
      ;; 画像データの取得
      (let* ((width     (~ img 'width))
             (height    (~ img 'height))
             (data      (~ img 'data))
             (data-size (* width height))
             (data-byte (size-of data)))
        (unless (>= width  0)
          (err "image width is invalid (width=~d)" width))
        (unless (>= height 0)
          (err "image height is invalid (height=~d)" height))
        (unless (>= data-byte (* width height 4))
          (err "image data size is too small (data-byte=~d)" data-byte))
        ;; ファイルヘッダーの書き込み
        (let* ((ftype        #x424D)
               (width-byte-0 (* width 3))
               (width-byte-1 (+ width-byte-0
                                (if (= (modulo width-byte-0 4) 0)
                                  0
                                  (- 4 (modulo width-byte-0 4)))))
               (fsize        (+ 14 40 (* width-byte-1 height)))
               (foffbits     (+ 14 40)))
          (pack "nVvvV"
                (list ftype fsize 0 0 foffbits)
                :output out)
          ;; 情報ヘッダーの書き込み
          (let ((isize        40)
                (iwidth       width)
                (iheight      height)
                (ibitcount    24)
                (icompression 0)
                (isizeimage   (* width-byte-1 height)))
            (pack "VV!V!vvVVV!V!VV"
                  (list isize iwidth iheight 1 ibitcount
                        icompression isizeimage 0 0 0 0)
                  :output out)
            ;; 画像データの書き込み
            (do ((j 0 j)
                 (k (* iwidth 4 (- iheight 1)) k)
                 (c 0 c)
                 (x 0 x))
                ;; 最後の1ラインも、4バイト境界まで0の書き込みが必要
                ;((>= c data-size) #f)
                ((>= j isizeimage) #f)
              (cond
               ;; 1ライン書き込み後は、4バイト境界まで0を書き込む
               ((>= x iwidth)
                (cond
                 ((= (modulo j 4) 0)
                  (set! x 0)
                  (set! k (- k (* iwidth 4 2))))
                 (else
                  (write-one-data 0 out)
                  (inc! j))))
               ;; 1ピクセル分のデータを書き込む
               (else
                (let ((r (get-one-data data k))
                      (g (get-one-data data (+ k 1)))
                      (b (get-one-data data (+ k 2)))
                      (a (get-one-data data (+ k 3))))
                  (write-one-data b out)
                  (write-one-data g out)
                  (write-one-data r out)
                  (set! j (+ j 3))
                  (set! k (+ k 4))
                  (inc! c)
                  (inc! x)
                  ))))
            )))
      ))
  )

