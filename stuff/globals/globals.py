"""
Implementing MUMPS' "globals" using MongoDB.

Author: Carlo Hamalainen <carlo.hamalainen@gmail.com>
Date: September 2010
"""

from pymongo import Connection, ASCENDING
import re

class MUMPSGlobal:
    def __init__(self, stuff):
        self.stuff = stuff

    def __convert_key(self, key):
        if type(key) == list:
            new_key = key
        elif type(key) != tuple:
            new_key = (key,)
        else:
            new_key = key

        return [str(k) for k in key]

    def __setitem__(self, _key, value):
        key = self.__convert_key(_key)

        for i in range(1, len(key) + 1):
            key_string = ','.join(key[:i]) + ','
            if not self.__contains__(key[:i]):
                doc = {"name": key_string}
                #print "inserting", doc
                self.stuff.update(doc, doc, upsert=True)

        self.stuff.update({"name": ','.join(key[:i]) + ','}, {"name": ','.join(key[:i]) + ',', "value": value}, upsert=True)

    def __getitem__(self, _key):
        key = self.__convert_key(_key)

        x = self.stuff.find_one({"name": ','.join(key) + ','})
        if x is None: raise ValueError, "No such node \"%s\"" % (','.join(key) + ',')
        return x["value"]

    def __contains__(self, _key):
        key = self.__convert_key(_key)
        return self.stuff.find_one({"name": ','.join(key) + ','}) is not None

    def children(self, _key):
        key = self.__convert_key(_key)
        key = ','.join([str(k) for k in key]) + ',' 
 
        for x in stuff.find({"name": {'$regex':'^' + key + '[^,]+,$'}}, fields=["name"], sort=[("name", ASCENDING)]):
            yield x

    def delete(self, _key):
        key = self.__convert_key(_key)
        key = ','.join([str(k) for k in key]) + ',' 
 
        for x in stuff.find({"name": {'$regex':'^' + key}}, fields=["name"], sort=[("name", ASCENDING)]):
            stuff.remove(x["_id"])

    def pretty_print(self):
        for x in self.stuff.find():
            if "value" in x: value = x["value"]
            else:            value = None

            print x["name"], '=', value


if __name__ == "__main__":
    connection = Connection()
    db = connection.test_globals

    stuff = db.stuff
    stuff.drop()
    stuff.ensure_index("name")

    """
    MUMPS code to set a few "globals":

    Set ^Employee("MGW","UK","London") = 2
    Set ^Employee("MGW","UK","London",1) = "Rob Tweed`Director`020 8404 3054"
    Set ^Employee("MGW","UK","London",2) = "Chris Munt`Director`01737 371457"
    """

    emp = MUMPSGlobal(stuff)
    emp["MGW", "UK", "London"] = 2
    emp["MGW", "UK", "London", 1] = "Rob Tweed`Director`020 8404 3054"
    emp["MGW", "UK", "London", 2] = "Chris Munt`Director`01737 371457"

    print "After setting three labels:"
    emp.pretty_print()
    print

    print "Testing existence of vertices:"
    for key in [("MGW", "UK", "London", 2), ("MGW", "UK", "Bristol")]:
        print "%s in emp: %s" % (key, str(key in emp))
    print

    print "Immediate children of vertices:"
    for key in [("MGW",), ("MGW", "UK",)]:
        print "children of %s: %s" % (key, str([str(x["name"]) for x in emp.children(key)]))
    print

    print 'Delete ("MGW", "UK") and then print all vertices:'
    emp.delete(("MGW", "UK"))
    emp.pretty_print()
    print
