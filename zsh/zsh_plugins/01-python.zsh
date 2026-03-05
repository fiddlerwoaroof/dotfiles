alias ..python="PYTHONPATH=.. python"
alias .python="PYTHONPATH=. python"

activate_env() {
  local venv=.
  if [[ -e bin/activate ]]; then
    echo "sourcing local env: `pwd`/bin/activate"
  elif [[ -e venv/bin/activate ]]; then
    echo "sourcing local env: `pwd`/venv/bin/activate"
    venv=venv
  elif [[ -e .venv/bin/activate ]]; then
    echo "sourcing local env: `pwd`/.venv/bin/activate"
    venv=.venv
  else
    env=$1
    pushd $HOME/python_envs/ > /dev/null
    venv="$PWD/$env"
    popd

    if [[ $env == "" ]]; then
      counter=1
      typeset -A choices
      unset choice
      for x in `ls "$venv"`; do
        echo $counter\) `basename $x`
        choices[$counter]=$x
        (( counter++ ))
      done
      echo -n "your choice? "
      choice=-1
      read choice
      if [[ $choice == "" ]]; then
        return
      fi
      venv="$venv/$choices[$choice]"
      echo "you chose $venv"
    fi
  fi
  source "$venv/bin/activate"
  unset venv env;
}
alias ae=activate_env

psc() {
  python -u -c "from __future__ import print_function; import sys;$1"
}

new_virtual_env() {
  virtualenv -p "python$2" --no-site-packages "$HOME/python_envs/$1"
  pushd "$HOME/python_envs/$1"
}
alias ne=new_virtual_env

ge() {
  cd "$VIRTUAL_ENV"
}
