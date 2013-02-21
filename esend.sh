#!/bin/bash

strlen () {
    str="`echo $1`"
    echo ${#str}
}

mkmsg () {
    echo "|`strlen "$1"`|$1"
}

print_usage () {
    echo $0: $1
    echo USAGE: $0 '[[host] port] message'
    echo You can also try
    echo ' $' $0 [[host] port] register
    echo and then
    echo ' $' $0 [[host] port] example
    echo to see an example.
}

check_arguments () {
    [[ $port =~ [0-9]+ ]]
}


which nc &> /dev/null
if [ ! $? -eq 0 ]
then
    echo "This utility needs the nc (netcat) command."
    exit 1
fi


case $# in
    1)
	host='localhost'
	port=5000
	msg=$1
	;;
    2)
	host='localhost'
	port=$1
	msg=$2
	;;
    3)
	host=$1
	port=$2
	msg=$3
	;;
    *)
	print_usage "wrong number of arguments $#"
	exit 1
	;;
esac

example_registration='(:register "esend.sh"
 :handler-fn (lambda (id data)
               (let ((buf (get-buffer-create "*esend.sh example*")))
                  (save-current-buffer
                    (set-buffer buf)
                    (erase-buffer)
                    (insert data)))))'

example_msg='(:id "esend.sh"
 :notification (:text "esend.sh"
                :face :success
                :mouse-1 (lambda nil (interactive) (switch-to-buffer "*esend.sh example*"))
                :help "Example tooltip")
 :data "Some example data")
'
foo=`nc -h 2>&1`
nc_impl=GNU
echo $foo | grep 'OpenBSD' > /dev/null && nc_impl=BSD
echo $foo | grep 'nmap' > /dev/null && nc_impl=NMAP
case $nc_impl in
    "BSD")
        NC="nc"
        ;;
    "NMAP")
        NC="nc"
        ;;
    "GNU")
        NC="nc -c"
        ;;
esac
check_arguments
case $msg in
    "register")
	msg=$example_registration
	;;
    "example")
	msg=$example_msg
	;;
esac
if [ $? -eq 0 ]
then
    m=`mkmsg "$msg"`
    echo Sending "'$m'" to $host:$port
    echo $m | $NC $host $port
else
    print_usage "wrong argument format: port = $port"
fi


# mkmsg $1 | nc
