#!/bin/bash
USER=$1
svn log svn+ssh://scm.bits.illinoisstate.edu/ | grep "$USER" | wc -l

