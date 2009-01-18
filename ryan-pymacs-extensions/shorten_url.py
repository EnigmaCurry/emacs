#!/usr/bin/env python
# -*- coding: utf-8 -*-

__author__ = "Ryan McGuire (ryan@enigmacurry.com)"
__date__   = "Mon Sep 15 12:27:14 2008"

import doctest
import urllib2
import re

def shorten_with_is_gd(url):
    """Shorten a URL with is.gd

    >>> shorten_with_is_gd('http://www.enigmacurry.com')
    'http://is.gd/FFP'

    """
    u = urllib2.urlopen("http://is.gd/api.php?longurl="+url)
    return u.read()

def shorten_in_text(text):
    """Shorten all the urls found inside some text

    >>> shorten_in_text('Hi from http://www.enigmacurry.com')
    'Hi from http://is.gd/FFP'
    
    """
    replacements = {} #URL -> is.gd URL
    #Only check for urls that start with "http://" for now
    for m in re.finditer("http://[^ \n\r]*", text):
        try:
            replacements[m.group()] = shorten_with_is_gd(m.group())
        except:
            replacements[m.group()] = m.group()
    for url,replacement in replacements.items():
        text = text.replace(url, replacement)
    return text

if __name__ == '__main__':
    doctest.testmod(verbose=True)
