if exists('g:loaded_jpf')
  finish
endif

command! -range=% -nargs=0 Jpf call jpf#replace()

let g:loaded_jpf = 1
