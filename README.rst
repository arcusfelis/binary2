binary2
=======

This library contains useful functions for manipulating binaries.

**License**: MIT

**Author**: Uvarov Michael (freeakk@gmail.com)

.. image:: https://secure.travis-ci.org/freeakk/binary2.png?branch=master
    :alt: Build Status
    :target: http://travis-ci.org/freeakk/binary2


Examples
--------

General:

.. code-block:: erlang

    1> binary2:reverse(<<1,2,3>>).
    <<3,2,1>>
    2> binary2:join([<<1,2,3>>, <<4,5,6>>, <<7,8,9>>], <<0>>).
    <<1,2,3,0,4,5,6,0,7,8,9>>
    3> binary2:suffix(<<1,2,3,4,5,6>>, 4).
    <<3,4,5,6>>
    4> binary2:suffix(<<1,2,3,4,5,6>>, 10).
    ** exception error: bad argument
    5> binary2:prefix(<<1,2,3,4,5,6>>, 4).                    
    <<1,2,3,4>>
    6> binary2:prefix(<<1,2,3,4,5,6>>, 10).
    ** exception error: bad argument

Trimming:

.. code-block:: erlang

    11> binary2:trim(<<0,1,2,3,0>>).        
    <<1,2,3>>
    12> binary2:trim(<<" test  ">>, $ ).
    <<"test">>
    13> binary2:ltrim(<<" test  ">>, $ ).
    <<"test  ">>
    14> binary2:rtrim(<<" test  ">>, $ ).
    <<" test">>


Bit-sets:

.. code-block:: erlang
    21> binary2:inverse(<<-1:7>>).
    <<0:7>>
    22> binary2:inverse(<<0:7>>). 
    <<127:7>>
    23> binary2:union(<<2:5>>, <<1:5>>).
    <<3:5>>
    24> binary2:subtract(<<-1:5>>, <<1:5>>).
    <<30:5>>
    25> binary2:intersection(<<3:5>>, <<5:5>>).
    <<1:5>>
