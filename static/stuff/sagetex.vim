" Vim syntax file
" Language:	   SageTeX 
" Maintainer:  Carlo Hamalainen <carlo.hamalainen at gmail.com>
" Installation:
"	To automatilcally load this file when a .tex file is opened, add the
"	following lines to ~/.vimrc
"
"		augroup filetypedetect
"			au! BufRead,BufNewFile *.tex		setfiletype sagetex
"		augroup END
"
"	You will have to restart vim for this to take effect.
"	
"   This file is heavily based on the ConTeXt vim highlighter
"   by Mojca Miklavec, posted here:
"   http://www.ntg.nl/pipermail/ntg-context/2005/011821.html
"
"   Bugs: For some reason this won't work if there are \begin{document}
"   commands in the file, so I just split up the LaTeX file as follows:
"   
"       \documentclass{article}
"       \usepackage{sagetex}
"       
"       \begin{document}
"       \input{body}
"       \end{document}
"
"   Then editing body.tex works ok.

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

"Source the tex syntax file
runtime! syntax/tex.vim
"Set the filetype to tex to load the tex %??? ftplugins
set ft=tex
unlet b:current_syntax

syn include @pythonTop syntax/python.vim

syn region pythonBlock matchgroup=pythonDelim start=#\\begin{sageblock}# end=#\\end{sageblock}# keepend contains=@pythonTop
syn region pythonBlock matchgroup=pythonDelim start=#\\begin{sagesilent}# end=#\\end{sagesilent}# keepend contains=@pythonTop
syn region pythonBlock matchgroup=pythonDelim start=#\\begin{sageverbatim}# end=#\\end{sageverbatim}# keepend contains=@pythonTop
syn region pythonBlock matchgroup=pythonDelim start=#\\begin{sagetexindent}# end=#\\end{sagetexindent}# keepend contains=@pythonTop
syn region pythonBlock matchgroup=pythonDelim start=#\\sage{# end=#}# keepend contains=@pythonTop
syn region pythonBlock matchgroup=pythonDelim start=#\\sageplot{# end=#}# keepend contains=@pythonTop

hi link pythonDelim texSTatement

let b:current_syntax = "sagetex"
