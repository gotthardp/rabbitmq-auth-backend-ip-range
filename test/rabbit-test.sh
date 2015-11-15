#!/bin/sh
CTL=$1

$CTL add_user "local-user" "pass"
$CTL set_permissions -p / "local-user" ".*" ".*" ".*"
$CTL set_user_tags "local-user" "ip-local"

$CTL add_user "private-user" "pass"
$CTL set_permissions -p / "private-user" ".*" ".*" ".*"
$CTL set_user_tags "private-user" "ip-private"
