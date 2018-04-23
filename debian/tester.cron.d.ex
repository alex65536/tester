#
# Regular cron jobs for the tester package
#
0 4	* * *	root	[ -x /usr/bin/tester_maintenance ] && /usr/bin/tester_maintenance
