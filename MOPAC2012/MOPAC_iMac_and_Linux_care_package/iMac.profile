# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

#
PS1="\w >"
# User specific aliases and functions
#
#  Aliases to do with navigation
#
alias g='cd ~/G'
alias gold='cd /mnt/gold'
alias home='cd ~'
alias Platform='cd ~/Platform'
alias protein='cd ~/M/proteins'
alias pwork='cd ~/PARAM_Platform'
alias soft='cd ~/MOPAC_Source/'
alias psoft='cd ~/PARAM_Source/'
alias tm='cd ~/G/tm'
alias up='cd ../'
alias ut='cd ~/utilities'
alias work='cd ~/MOPAC_Platform'
#
# Aliases for commands
#
alias cls='clear'
alias d=' ~/utilities/dir.csh'
alias darc='ls -gts | grep "\.arc" | head -40'
alias ddat='ls -gts | grep "\.dat" | head -40'
alias ddir='ls -ls | grep drw'
alias del='rm'
alias dir=' ls -ls '
alias dirnew='~/utilities/dir.csh'
alias dout='ls -gts | grep "\.out" | head -40'
alias dmop='ls -gts | grep "\.mop" | head -40'
alias exportexe='~/utilities/export_exe.csh'
alias exportout='~/utilities/export_out.csh'
alias f90='/opt/intel/fc/10.1.008/bin/ifort'
alias gettests='~/utilities/get_tests.csh'
alias jmol='/Applications/jmol-11.6/jmol.sh'
alias m='~/utilities/make_mopac.csh'
alias mnt='~/utilities/mnt.csh'
alias mounted='~/utilities/mounted.csh'
alias mopac='~/utilities/run_mopac.csh'
alias opt='~/utilities/opt.csh'
alias p='~/utilities/make_param.csh'
alias param='~/utilities/run_param.csh'
alias running='~/utilities/running.csh'
alias shut='~/utilities/shut.csh'
alias ty=cat
alias umnt='~/utilities/umnt.csh'
alias update='~/utilities/update_mopac.csh'
alias updatep='~/utilities/update_param.csh'
alias updatedata='~/utilities/update_param_data.csh'
alias vi='~/utilities/vi.csh'
alias watch='~/utilities/watch.csh'
#
# Export standard Environmental Variables
#
export MOPAC_LICENSE=/opt/mopac
export PATH=$PATH:~/:./
#
