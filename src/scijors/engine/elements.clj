(ns scijors.engine.elements
  (:use [scijors.engine.grammar])
  (:require [instaparse.core :as insta]
            [clojure.string :as strings]))

(defgrammar elements-grammar "
<ws> = #'\\s+';
<backslash> = '\\\\';
<qsym> = <backslash> #'[a-zA-Z-_0-9\\$?!+*]+' <backslash> ;
<qns> = <backslash> #'([a-zA-Z-_0-9\\$?!+*]+\\.)+[a-zA-Z-_0-9\\$?!+*]+' <backslash> ;
<qns-sym> = <backslash> #'([a-zA-Z-_0-9\\$?!+*]+\\.)*[a-zA-Z-_0-9\\$?!+*]+/[a-zA-Z-_0-9\\$?!+*]+' <backslash>;
<usym> = #'[a-zA-Z_\\$][a-zA-Z-_0-9\\$?!]*';
<uns> = #'([a-zA-Z_\\$][a-zA-Z-_0-9\\$?!]*\\.)+[a-zA-Z-_\\$][a-zA-Z-_0-9\\$?!]*';
<uns-sym> = #'([a
-zA-Z_\\$][a-zA-Z-_0-9\\$?!]*\\.)*[a-zA-Z_\\$][a-zA-Z-_0-9\\$?!]*/[a-zA-Z-_\\$][a-zA-Z-_0-9\\$?!]*';
<sym> = usym | qsym;
<ns> = uns | qns;
<ns-sym> = uns-sym | qns-sym;
<dquote> = '\"';
<lsqparen> = ws? '[' ws?;
<rsqparen> = ws? ']' ws?;
<lparen> = ws? '(' ws?;
<rparen> = ws? ')' ws?;
<lbrace> = ws? '{' ws?;
<hashlbrace> = ws? '#{' ws?;
<rbrace> = ws? '}' ws?;
<tag-open> = '{%' ws?;

<tag-close> = ws? ('%}' | '}');
<comma> = ws? ',' ws?;
<colon> = ws? ':' !usym ws?;
<pipe> = ws? '|' !'|' ws?;
<clj-ws> = comma | ws;
<dbl-lbrace> = '{{' !'{' ws?;
<dbl-rbrace> = ws? '}}';
<end> = <tag-open> <'end'> <tag-close>;
<string> =  <dquote> #'(\\\\\"|[^\"])*' <dquote>;

")

(defn unescape-string [s]
  (-> s (strings/replace "\\\"" "\""))
  )
