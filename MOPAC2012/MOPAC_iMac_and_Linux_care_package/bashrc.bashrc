# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

#
PS1="\w >"
# MOPAC_LICENSE='/opt/mopac'
# User specific aliases and functions
#  Aliases to do with navigation
#
alias g='cd ~/G'
alias gold='cd /mnt/gold'
alias home='cd ~'
alias protein='cd ~/M/proteins'
alias pwork='cd ~/PARAM_Work'
alias soft='cd ~/MOPAC_Source/'
alias psoft='cd ~/PARAM_Source/'
alias tm='cd ~/G/tm'
alias up='cd ../'
alias ut='cd ~/utilities'
alias work='cd ~/MOPAC_Work'
#
# Aliases for commands
#
alias clean='~/utilities/clean.csh'
alias cls='clear'
alias d=' ~/utilities/dir.csh'
alias darc='ls -g -t -s *.arc'
alias ddat=' ls -g -t -u *.dat'
alias ddir='ls -ls | grep drw'
alias del='rm'
alias dir=' ls -ls '
alias dirnew='~/utilities/dir.csh'
alias dout='ls -g -t -s *.out'
alias dmop='ls -g -t -s *.mop'
alias exportexe='~/utilities/export_exe.csh'
alias exportout='~/utilities/export_out.csh'
alias f90='/opt/intel/fc/10.1.008/bin/ifort'
alias gettests='~/utilities/get_tests.csh'
alias jmol='/Applications/jmol-11.6/jmol.sh'
alias m='~/utilities/make_mopac.csh'
alias md='~/utilities/make_mopac_debug.csh'
alias p='~/utilities/make_param.csh'
alias mnt='~/utilities/mnt.csh'
alias mopac='~/utilities/run_mopac.csh'
alias param='~/utilities/run_param.csh'
alias running='ps -ax | grep MOP | grep User | grep exe'
alias shut='~/utilities/shut.csh'
alias umnt='~/utilities/umnt.csh'
alias update='~/utilities/update_mopac.csh'
alias updatep='~/utilities/update_param.csh'
alias vi='~/utilities/vi.csh'
alias watch='~/utilities/watch.csh'
#
# Export standard Environmental Variables
#
export MOPAC_LICENSE=/opt/mopac
export PATH=$PATH:~/:./
#
