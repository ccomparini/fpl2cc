#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
  esml - Even Simpler Minimal Language

  The idea behind this is to make it as simple as possible to write
  and compare data dumps.  For example, if you have 1000 elements and
  only the 534th ones differ, a simple command line diff will show
  exactly those elements.  Same with nested structures - if there are
  deeply nested structures, even very deep differences are easy to find
  and examine using standard unix command line tools.

  None of json, yaml, nor toml could give me anything satisfactory in
  this regard.

  Key features:
   - trailing commas are allowed
   - multiline strings are allowed (in fact, required if there's a newline)
   - mixed type arrays work
   - everything is flattened:
     - each element of each dict gets its own line with the
       full path to the element.  eg:
           { 'foo': 'bar', 'bat': { 'a': 1, 'wup': 23, 'moo': 'hi' } }
       becomes:
           bat.a = 1
           bat.moo = 'hi'
           bat.wup = 23
           foo = bar
       (note the sorting and that each assignment gets its own line)
     - each array item gets its own line, including the array subscript
       (in square brackets)
     - None/missing values are valid (and represented by nothing).
       example:
           { 'foo': None }
       becomes:
           foo = 

   Disadvantages:
     - output is bulkier than json, toml, and pretty much anything else
     - I made this up so there's no off-the-shelf parser for python.
       However, it's very easy to parse - everything is
           <nested key> = [value]
       .. and, of course (per the goal) just about any unix command line
       tool will "parse" it for the principle purposes.

"""
import unittest

#
# New NEW plan is to blow off both json and toml, because
# neither of them is suitable for what I want.
#
# Why this?
#   - stderr, stdout, return codes, and anything else can all go
#     in one output file per test; hence less clutter.
#   - Unlike json (which I tried first), toml will let me do multiline
#     strings, which is much easier to read and diff.
#   - Unlike toml (which I tried next), common line-based tools
#     (diff, grep, etc) will work relatively well for analysing
#     the output.
#   - As with toml, I'd have to write a dumper anyway, so for this
#     context it's not any significant reinvention.
#

def _format_value(val):
    if val is None:
        return ""
    elif isinstance(val, bool):
        return "true" if val else "false"
    elif isinstance(val, (int, float)):
        return str(val)
    elif isinstance(val, str):
        # the only thing escaped in strings is quotes and the escape char
        # itself.  preserve newlines. upon reading an esml string, we can
        # replace exactly '\"' with '"' and then exactly r'\\' with '\\'.
        # nicht war?  am I having a think-o?
        # (later note:  no think-o, runs good)
        val = val.replace('\\', r'\\');
        val = val.replace('"',  r'\"');
        return f'"{val}"'
    elif isinstance(val, bytes):
        # doing a utf-8 encoding w/ backslashed hex codes for binary,
        # since it's the most transparent thing I can think of which
        # fits the whole goal of being able to process the resulting
        # files with standard unix command line tools (diff, grep etc).
        return _format_value(val.decode(errors='backslashreplace'))
    else:
        # we actually want to support as much as possible, so if we hit
        # this it's a sign we need to support more (maybe by defaulting
        # to string conversion) and not a design feature:
        raise TypeError(f"{type(val).__name__} {val!r} is not supported")

def dumps(value, name = ""):
    """
        dumps(value) - convert the dictionary or list passed to an esml string
        dumps(value, name="name") - convert the value passed to an esml string

        In the first invocation, if the value passed is a list, the "keys" in
        the output will be square bracketed indexes into the list.

        The second invocation is necessary for writing non-compound types due
        to the fact that in esml everything is key/value.
    """

    out = []
    if isinstance(value, dict):
        if name:
            name += '.'
        for subname in sorted(value.keys()):
            out.append(dumps(value[subname], name=f"{name}{subname}"))
    elif isinstance(value, list):
        for index in range(0, len(value)):
            out.append(dumps(value[index], name=f"{name}[{index}]"))
    else:
        if not name:
            raise ValueError("Can't dump " + type(value).__name__ + " - every value in esml must have a name")
        return f"{name} = {_format_value(value)}\n"

    return ''.join(out)


class _Testesml(unittest.TestCase):

    def test_all(self):
        expected = (
            'foo = "bar\n\\"bat\\"\n"\n'
            'lat = 23.4394\n'
            'not_utf-8 = "\\\\xff🙂\0\\"\r\n#"\n'
            'wup.flew = false\n'
            'wup.peep.deep[0] = 2\n'
            'wup.peep.deep[1] = 3\n'
            'wup.shoe = true\n'
            'zoo[0].kind = "cow"\n'
            'zoo[0].says = "moo"\n'
            'zoo[1].kind = "fox"\n'
            'zoo[1].says = \n'
        )
        #print(expected)
        #print("==== above is the expected poop ====")

        got = dumps({
            'foo': 'bar\n"bat"\n',
            'zoo': [
                { 'kind': 'cow', 'says': 'moo'},
                { 'kind': 'fox', 'says': None},
            ],
            'wup': {
                'shoe': True, 'flew': False, 'peep': {
                    'deep': [ 2, 3 ]
                }
            },
            'not_utf-8': bytes([
                0xff,                   # garbage
                0xf0, 0x9f, 0x99, 0x82, # utf-8 smiley
                0x00,                   # c end-of-string
                0x22,                   # ascii double quote
                0x0d, 0x0a,             # microsoft nl - expect to see \r \n
                0x23                    # '#'
            ]),
            'lat': 23.4394,
        })

        # I'm calling it "good enough" to test multiline-ness and
        # nesting in one go.  This module is, itself, intended for
        # testing so it'll get plenty exercised at compile time.
        self.assertEqual(got, expected)

        # everything in esml needs a name. if you pass it an array
        # or a dict, the names of all the values can be inferred,
        # but if not you must give it a name:
        with self.assertRaises(ValueError):
            dumps('oh hai')

        # (check if it dtrt if the top level thing is an array)
        self.assertEqual(
            dumps([ {
                'mixed_array': [ 23, 'fruitbat' ],
                'flop': '\nwup'
            }]), (
                '[0].flop = "\n'
                'wup"\n'
                '[0].mixed_array[0] = 23\n'
                '[0].mixed_array[1] = "fruitbat"\n'
            )
        )

if __name__ == '__main__':
    unittest.main()


