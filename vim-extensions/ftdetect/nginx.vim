au BufRead,BufNewFile *.nginx set ft=nginx
au BufRead,BufNewFile */etc/nginx/* set ft=nginx
au BufRead,BufNewFile */usr/local/nginx/conf/* set ft=nginx
au BufRead,BufNewFile nginx.conf set ft=nginx

au BufRead,BufNewFile time.md nmap \td o<Esc>:.!date +"-- \%A \%Y-\%m-\%d"<C-m>:w<C-m>
au BufRead,BufNewFile time.md nmap \ts o<Esc>:.!date +"   start@\%H:\%M:\%S--"<C-m>:w<C-m>
au BufRead,BufNewFile time.md nmap \te <Esc>:.!date +"`tee`\%H:\%M:\%S"<C-m>:w<C-m>
au BufRead,BufNewFile time.md nmap \tc <Esc>:.!date +"`tee`,\%H:\%M:\%S--"<C-m>:w<C-m>
