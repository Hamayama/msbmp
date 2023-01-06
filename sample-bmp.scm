;;
;; Windows bitmap file load and save sample
;;

(add-load-path "." :relative)
(use msbmp)

(define file1 "image0001.bmp")
(define file2 "image0001_out.bmp")

;; load
(define img1 (load-ms-bitmap-file file1))

;; pixel data   R   G   B   A
(define data1 '(255 255 255 0
                255 0   0   0
                0   255 0   0
                0   0   255 0))

;; overwrite some pixels
(dotimes (i (length data1))
  (set! (~ img1 'data i) (~ data1 i)))

;; save
(save-ms-bitmap-file file2 img1)

