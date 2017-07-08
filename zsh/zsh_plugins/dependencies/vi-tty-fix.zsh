alias vi='vim'
vim() {
    stty -ixon
    env vim $*
    stty ixany
}
