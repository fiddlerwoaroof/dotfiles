function battery_charge() {
  # the -S is for performance
  python -S "$HOME/bin/batcharge.py" 2>/dev/null
}
    
    
