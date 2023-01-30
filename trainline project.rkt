;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |trainline project|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;#lang racket
(define-struct trainline (name exits))
; trainline is (make-trainline string (list of trainline))
;interpretuation . the trainlines name and list of trainlines that the exist leads too


;Hendon -> angle

(define line1 (make-trainline "hendon" (list (make-trainline "angle" empty))))


(define line2
  (shared ((-0- (make-trainline "hendon" (list (make-trainline "angle" (list -0-)))))
           )-0-))

