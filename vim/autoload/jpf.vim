let s:save_cpo = &cpo
set cpo&vim

function! s:system(str, ...) abort
  let command = a:str
  let input = a:0 >= 1 ? a:1 : ''

  if a:0 == 0 || a:1 ==# ''
    let output = vimproc#system(command)
  elseif a:0 == 1
    let output = vimproc#system(command, input)
  else
    let output = vimproc#system(command, input, a:2)
  endif

  return output
endfunction

function! s:success(result) abort
  let success = vimproc#get_last_status() == 0
  return success
endfunction

function! s:fail(err) abort
  echoerr 'jpf has failed to format.'
  echohl ErrorMsg
  for l in split(a:err, "\n")[0:1]
    echomsg l
  endfor
  echohl None
endfunction

function! s:verify() abort
  if !executable("jpf")
    echoerr "jpf is not found."
  endif
endfunction

function! jpf#format() abort
  let jpf = 'jpf'
  let txt = join(getline(1, '$'), "\n")
  return s:system(jpf, txt)
endfunction

function! jpf#replace() abort
  call s:verify()

  let pos_save = a:0 >= 1 ? a:1 : getpos('.')
  let formatted = jpf#format()
  if !s:success(formatted)
    call s:fail(formatted)
    return
  endif

  let winview = winsaveview()
  let splitted = split(formatted, '\n')

  silent! undojoin
  if line('$') > len(splitted)
      execute len(splitted) .',$delete' '_'
  endif
  call setline(1, splitted)
  call winrestview(winview)
  call setpos('.', pos_save)
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
