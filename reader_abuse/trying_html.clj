(ns reader-abuse.trying-html
    (:require reader-abuse.iexprs)
    (:require reader-abuse.infix)
    (:require reader-abuse.hook)
    (:use [org.apache.commons.lang.StringEscapeUtils :only escapeHtml]))

(do #I
    defn tag [tag-name, self-closing]
      fn [& args]
        if map? (first args)
           formatter tag-name, (first args),
                     (and self-closing, apply str (rest args))
           formatter tag-name, {}
                     (and self-closing, (apply str args))
    
    def escape escapeHtml
    
    defn format-attrs [attrs]
      apply str
            map
              fn [[attr-name value]]
                str " ", attr-name, "=\"", (escape value), "\""
              attrs
    
    defn format-tag [tag-name, attrs, contents]
      "Produces the html representation of an element. Self-closing tags
      should have nil `contents`, tags that are just empty should have \"\"."
      
      str "<", tag-name,
          format-attrs attrs
          if contents
             str ">", contents, "</", tag-name, ">"
             " />"
    
    def regular-tags
      '(html head title script style link body p a h1 h2 h3 h4 h5 h6 div span)
    
    def self-closing-tags
      '(img br hr)
    
    def all-tags
      concat regular-tags self-closing-tags
    
    domacro
      list `do
        for [tag-symbol regular-tags]
          syntax-quote
            def ~tag-symbol
                tag ~(string tag-symbol), false
        for [tag-symbol self-closing-tags]
          syntax-quote
            def ~tag-symbol
                tag ~(string tag-symbol), true
    
    def tag-mappings
      list `let
        vec
          ~@()
    
    ; could make an expotable html which
    ; `lets` all of the other tags for its contents
    
    defmacro html-doc [& contents]
      syntax-quote
        str
          "<!doctype html>"
          let [~'html html
               ~'body body
               ~'title title
               ~'head head
               ~'p p
               ~'a a]
            ~@contents
    
    defmacro domacro [& body]
      syntax-quote
        defmacro donemacro_# []
          ~@body
        (donemacro_#)
    
    def html (tag "html", fn [_, attributes])
    
    str
      "<!doctype html>"
      html
        head
          title "Hello World"
        "Go to "
        body
          p
            a {href "http://google.com/"} "Google"



)