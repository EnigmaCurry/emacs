#!/usr/bin/env python
# -*- coding: utf-8 -*-

__author__ = "Ryan McGuire (ryan@enigmacurry.com)"
__date__   = "Sun Jan 25 12:22:51 2009"

"""This is a Wordpress client library made specifically to support an Emacs mode
for writing blogs. This is not very Pythonic AT ALL. It's made to get around the
idiosyncrasies of Pymacs (eg, Pymacs can't read an eval'd dictionary)"""

import xmlrpclib
import operator

class WordpressClient:
    def __init__(self, url, username, password, blog_id = 0):
        self.url = url  
        self.username = username
        self.password = password
        self.blog_id = blog_id
        self.server = xmlrpclib.ServerProxy(self.url)
        self.posts = {} # post id -> post dict
        self.categories = {}

    def update(self):
        self.__refresh_posts()
        self.__refresh_categories()
        
    def __refresh_posts(self):
        posts = self.server.metaWeblog.getRecentPosts(self.blog_id, self.username, self.password, 0)
        for p in posts:
            self.posts[p['postid']] = p
            
    def __refresh_categories(self):
        self.categories = self.server.metaWeblog.getCategories(self.blog_id, self.username, self.password)
        
    def get_titles_published(self):
        return [[post['postid'], post['title']] for post in \
                    sorted(self.posts.values(), key=operator.itemgetter('dateCreated'), reverse=True) \
                    if post['post_status'] == 'publish']
    
    def get_titles_draft(self):
        return [[post['postid'], post['title']] for post in \
                    sorted(self.posts.values(), key=operator.itemgetter('dateCreated'), reverse=True) \
                    if post['post_status'] == 'draft']
                    
    def get_post_categories(self, post_id):
        post = self.posts[str(post_id)]
        return post['categories']

    def get_post_title(self, post_id):
        post = self.posts[str(post_id)]
        return post['title']
    
# Local Variables:
# pymacs-auto-reload: t
# End:
