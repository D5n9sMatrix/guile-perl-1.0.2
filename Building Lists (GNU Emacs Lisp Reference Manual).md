<!DOCTYPE html>
<!-- saved from url=(0077)https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Lists.html -->
<html><!-- Created by GNU Texinfo 7.0.3, https://www.gnu.org/software/texinfo/ --><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8">

<title>Building Lists (GNU Emacs Lisp Reference Manual)</title>

<meta name="description" content="Building Lists (GNU Emacs Lisp Reference Manual)">
<meta name="keywords" content="Building Lists (GNU Emacs Lisp Reference Manual)">
<meta name="resource-type" content="document">
<meta name="distribution" content="global">
<meta name="Generator" content="makeinfo">
<meta name="viewport" content="width=device-width,initial-scale=1">

<link rev="made" href="mailto:bug-gnu-emacs@gnu.org">
<link rel="icon" type="image/png" href="https://www.gnu.org/graphics/gnu-head-mini.png">
<meta name="ICBM" content="42.256233,-71.006581">
<meta name="DC.title" content="gnu.org">
<style type="text/css">
@import url('/software/emacs/manual.css');
</style>
</head>

<body lang="en">
<div class="section-level-extent" id="Building-Lists">
<div class="nav-panel">
<p>
Next: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Variables.html" accesskey="n" rel="next">Modifying List Variables</a>, Previous: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Elements.html" accesskey="p" rel="prev">Accessing Elements of Lists</a>, Up: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Lists.html" accesskey="u" rel="up">Lists</a> &nbsp; [<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents" title="Table of contents" rel="contents">Contents</a>][<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Index.html" title="Index" rel="index">Index</a>]</p>
</div>
<hr>
<h3 class="section" id="Building-Cons-Cells-and-Lists">5.4 Building Cons Cells and Lists</h3>
<a class="index-entry-id" id="index-cons-cells"></a>
<a class="index-entry-id" id="index-building-lists"></a>

<p>Many functions build lists, as lists reside at the very heart of Lisp.
<code class="code">cons</code> is the fundamental list-building function; however, it is
interesting to note that <code class="code">list</code> is used more times in the source
code for Emacs than <code class="code">cons</code>.
</p>
<dl class="first-deffn first-defun-alias-first-deffn">
<dt class="deffn defun-alias-deffn" id="index-cons"><span class="category-def">Function: </span><span><strong class="def-name">cons</strong> <var class="def-var-arguments">object1 object2</var><a class="copiable-link" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Lists.html#index-cons"> ¶</a></span></dt>
<dd><p>This function is the most basic function for building new list
structure.  It creates a new cons cell, making <var class="var">object1</var> the
<small class="sc">CAR</small>, and <var class="var">object2</var> the <small class="sc">CDR</small>.  It then returns the new
cons cell.  The arguments <var class="var">object1</var> and <var class="var">object2</var> may be any
Lisp objects, but most often <var class="var">object2</var> is a list.
</p>
<div class="example">
<div class="group"><pre class="example-preformatted">(cons 1 '(2))
     ⇒ (1 2)
</pre></div><div class="group"><pre class="example-preformatted">(cons 1 '())
     ⇒ (1)
</pre></div><div class="group"><pre class="example-preformatted">(cons 1 2)
     ⇒ (1 . 2)
</pre></div></div>

<a class="index-entry-id" id="index-consing"></a>
<p><code class="code">cons</code> is often used to add a single element to the front of a
list.  This is called <em class="dfn">consing the element onto the list</em>.
<a class="footnote" id="DOCF5" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Lists.html#FOOT5"><sup>5</sup></a>
For example:
</p>
<div class="example">
<pre class="example-preformatted">(setq list (cons newelt list))
</pre></div>

<p>Note that there is no conflict between the variable named <code class="code">list</code>
used in this example and the function named <code class="code">list</code> described below;
any symbol can serve both purposes.
</p></dd></dl>

<dl class="first-deffn first-defun-alias-first-deffn">
<dt class="deffn defun-alias-deffn" id="index-list"><span class="category-def">Function: </span><span><strong class="def-name">list</strong> <var class="def-var-arguments">&amp;rest objects</var><a class="copiable-link" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Lists.html#index-list"> ¶</a></span></dt>
<dd><p>This function creates a list with <var class="var">objects</var> as its elements.  The
resulting list is always <code class="code">nil</code>-terminated.  If no <var class="var">objects</var>
are given, the empty list is returned.
</p>
<div class="example">
<div class="group"><pre class="example-preformatted">(list 1 2 3 4 5)
     ⇒ (1 2 3 4 5)
</pre></div><div class="group"><pre class="example-preformatted">(list 1 2 '(3 4 5) 'foo)
     ⇒ (1 2 (3 4 5) foo)
</pre></div><div class="group"><pre class="example-preformatted">(list)
     ⇒ nil
</pre></div></div>
</dd></dl>

<dl class="first-deffn first-defun-alias-first-deffn">
<dt class="deffn defun-alias-deffn" id="index-make_002dlist"><span class="category-def">Function: </span><span><strong class="def-name">make-list</strong> <var class="def-var-arguments">length object</var><a class="copiable-link" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Lists.html#index-make_002dlist"> ¶</a></span></dt>
<dd><p>This function creates a list of <var class="var">length</var> elements, in which each
element is <var class="var">object</var>.  Compare <code class="code">make-list</code> with
<code class="code">make-string</code> (see <a class="pxref" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Strings.html">Creating Strings</a>).
</p>
<div class="example">
<div class="group"><pre class="example-preformatted">(make-list 3 'pigs)
     ⇒ (pigs pigs pigs)
</pre></div><div class="group"><pre class="example-preformatted">(make-list 0 'pigs)
     ⇒ nil
</pre></div><div class="group"><pre class="example-preformatted">(setq l (make-list 3 '(a b)))
     ⇒ ((a b) (a b) (a b))
(eq (car l) (cadr l))
     ⇒ t
</pre></div></div>
</dd></dl>

<dl class="first-deffn first-defun-alias-first-deffn">
<dt class="deffn defun-alias-deffn" id="index-append"><span class="category-def">Function: </span><span><strong class="def-name">append</strong> <var class="def-var-arguments">&amp;rest sequences</var><a class="copiable-link" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Lists.html#index-append"> ¶</a></span></dt>
<dd><a class="index-entry-id" id="index-copying-lists"></a>
<p>This function returns a list containing all the elements of
<var class="var">sequences</var>.  The <var class="var">sequences</var> may be lists, vectors,
bool-vectors, or strings, but the last one should usually be a list.
All arguments except the last one are copied, so none of the arguments
is altered.  (See <code class="code">nconc</code> in <a class="ref" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Rearrangement.html">Functions that Rearrange Lists</a>, for a way to join
lists with no copying.)
</p>
<p>More generally, the final argument to <code class="code">append</code> may be any Lisp
object.  The final argument is not copied or converted; it becomes the
<small class="sc">CDR</small> of the last cons cell in the new list.  If the final argument
is itself a list, then its elements become in effect elements of the
result list.  If the final element is not a list, the result is a
dotted list since its final <small class="sc">CDR</small> is not <code class="code">nil</code> as required
in a proper list (see <a class="pxref" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Cons-Cells.html">Lists and Cons Cells</a>).
</p></dd></dl>

<p>Here is an example of using <code class="code">append</code>:
</p>
<div class="example">
<div class="group"><pre class="example-preformatted">(setq trees '(pine oak))
     ⇒ (pine oak)
(setq more-trees (append '(maple birch) trees))
     ⇒ (maple birch pine oak)
</pre></div><pre class="example-preformatted">
</pre><div class="group"><pre class="example-preformatted">trees
     ⇒ (pine oak)
more-trees
     ⇒ (maple birch pine oak)
</pre></div><div class="group"><pre class="example-preformatted">(eq trees (cdr (cdr more-trees)))
     ⇒ t
</pre></div></div>

<p>You can see how <code class="code">append</code> works by looking at a box diagram.  The
variable <code class="code">trees</code> is set to the list <code class="code">(pine oak)</code> and then the
variable <code class="code">more-trees</code> is set to the list <code class="code">(maple birch pine
oak)</code>.  However, the variable <code class="code">trees</code> continues to refer to the
original list:
</p>
<div class="example smallexample">
<div class="group"><pre class="example-preformatted">more-trees                trees
|                           |
|     --- ---      --- ---   -&gt; --- ---      --- ---
 --&gt; |   |   |--&gt; |   |   |--&gt; |   |   |--&gt; |   |   |--&gt; nil
      --- ---      --- ---      --- ---      --- ---
       |            |            |            |
       |            |            |            |
        --&gt; maple    --&gt;birch     --&gt; pine     --&gt; oak
</pre></div></div>

<p>An empty sequence contributes nothing to the value returned by
<code class="code">append</code>.  As a consequence of this, a final <code class="code">nil</code> argument
forces a copy of the previous argument:
</p>
<div class="example">
<div class="group"><pre class="example-preformatted">trees
     ⇒ (pine oak)
</pre></div><div class="group"><pre class="example-preformatted">(setq wood (append trees nil))
     ⇒ (pine oak)
</pre></div><div class="group"><pre class="example-preformatted">wood
     ⇒ (pine oak)
</pre></div><div class="group"><pre class="example-preformatted">(eq wood trees)
     ⇒ nil
</pre></div></div>

<p>This once was the usual way to copy a list, before the function
<code class="code">copy-sequence</code> was invented.  See <a class="xref" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequences-Arrays-Vectors.html">Sequences, Arrays, and Vectors</a>.
</p>
<p>Here we show the use of vectors and strings as arguments to <code class="code">append</code>:
</p>
<div class="example">
<div class="group"><pre class="example-preformatted">(append [a b] "cd" nil)
     ⇒ (a b 99 100)
</pre></div></div>

<p>With the help of <code class="code">apply</code> (see <a class="pxref" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Calling-Functions.html">Calling Functions</a>), we can append
all the lists in a list of lists:
</p>
<div class="example">
<div class="group"><pre class="example-preformatted">(apply 'append '((a b c) nil (x y z) nil))
     ⇒ (a b c x y z)
</pre></div></div>

<p>If no <var class="var">sequences</var> are given, <code class="code">nil</code> is returned:
</p>
<div class="example">
<div class="group"><pre class="example-preformatted">(append)
     ⇒ nil
</pre></div></div>

<p>Here are some examples where the final argument is not a list:
</p>
<div class="example">
<pre class="example-preformatted">(append '(x y) 'z)
     ⇒ (x y . z)
(append '(x y) [z])
     ⇒ (x y . [z])
</pre></div>

<p>The second example shows that when the final argument is a sequence but
not a list, the sequence’s elements do not become elements of the
resulting list.  Instead, the sequence becomes the final <small class="sc">CDR</small>, like
any other non-list final argument.
</p>
<dl class="first-deffn first-defun-alias-first-deffn">
<dt class="deffn defun-alias-deffn" id="index-copy_002dtree"><span class="category-def">Function: </span><span><strong class="def-name">copy-tree</strong> <var class="def-var-arguments">tree &amp;optional vecp</var><a class="copiable-link" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Lists.html#index-copy_002dtree"> ¶</a></span></dt>
<dd><p>This function returns a copy of the tree <var class="var">tree</var>.  If <var class="var">tree</var> is a
cons cell, this makes a new cons cell with the same <small class="sc">CAR</small> and
<small class="sc">CDR</small>, then recursively copies the <small class="sc">CAR</small> and <small class="sc">CDR</small> in the
same way.
</p>
<p>Normally, when <var class="var">tree</var> is anything other than a cons cell,
<code class="code">copy-tree</code> simply returns <var class="var">tree</var>.  However, if <var class="var">vecp</var> is
non-<code class="code">nil</code>, it copies vectors too (and operates recursively on
their elements).
</p></dd></dl>

<dl class="first-deffn first-defun-alias-first-deffn">
<dt class="deffn defun-alias-deffn" id="index-flatten_002dtree"><span class="category-def">Function: </span><span><strong class="def-name">flatten-tree</strong> <var class="def-var-arguments">tree</var><a class="copiable-link" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Lists.html#index-flatten_002dtree"> ¶</a></span></dt>
<dd><p>This function returns a “flattened” copy of <var class="var">tree</var>, that is,
a list containing all the non-<code class="code">nil</code> terminal nodes, or leaves, of
the tree of cons cells rooted at <var class="var">tree</var>.  Leaves in the returned
list are in the same order as in <var class="var">tree</var>.
</p></dd></dl>

<div class="example">
<pre class="example-preformatted">(flatten-tree '(1 (2 . 3) nil (4 5 (6)) 7))
    ⇒(1 2 3 4 5 6 7)
</pre></div>

<dl class="first-deffn first-defun-alias-first-deffn">
<dt class="deffn defun-alias-deffn" id="index-ensure_002dlist"><span class="category-def">Function: </span><span><strong class="def-name">ensure-list</strong> <var class="def-var-arguments">object</var><a class="copiable-link" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Lists.html#index-ensure_002dlist"> ¶</a></span></dt>
<dd><p>This function returns <var class="var">object</var> as a list.  If <var class="var">object</var> is
already a list, the function returns it; otherwise, the function
returns a one-element list containing <var class="var">object</var>.
</p>
<p>This is usually useful if you have a variable that may or may not be a
list, and you can then say, for instance:
</p>
<div class="example lisp">
<pre class="lisp-preformatted">(dolist (elem (ensure-list foo))
  (princ elem))
</pre></div>
</dd></dl>

<dl class="first-deffn first-defun-alias-first-deffn">
<dt class="deffn defun-alias-deffn" id="index-number_002dsequence"><span class="category-def">Function: </span><span><strong class="def-name">number-sequence</strong> <var class="def-var-arguments">from &amp;optional to separation</var><a class="copiable-link" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Lists.html#index-number_002dsequence"> ¶</a></span></dt>
<dd><p>This function returns a list of numbers starting with <var class="var">from</var> and
incrementing by <var class="var">separation</var>, and ending at or just before
<var class="var">to</var>.  <var class="var">separation</var> can be positive or negative and defaults
to 1.  If <var class="var">to</var> is <code class="code">nil</code> or numerically equal to <var class="var">from</var>,
the value is the one-element list <code class="code">(<var class="var">from</var>)</code>.  If <var class="var">to</var> is
less than <var class="var">from</var> with a positive <var class="var">separation</var>, or greater than
<var class="var">from</var> with a negative <var class="var">separation</var>, the value is <code class="code">nil</code>
because those arguments specify an empty sequence.
</p>
<p>If <var class="var">separation</var> is 0 and <var class="var">to</var> is neither <code class="code">nil</code> nor
numerically equal to <var class="var">from</var>, <code class="code">number-sequence</code> signals an
error, since those arguments specify an infinite sequence.
</p>
<p>All arguments are numbers.
Floating-point arguments can be tricky, because floating-point
arithmetic is inexact.  For instance, depending on the machine, it may
quite well happen that <code class="code">(number-sequence 0.4 0.6 0.2)</code> returns
the one element list <code class="code">(0.4)</code>, whereas
<code class="code">(number-sequence 0.4 0.8 0.2)</code> returns a list with three
elements.  The <var class="var">n</var>th element of the list is computed by the exact
formula <code class="code">(+ <var class="var">from</var> (* <var class="var">n</var> <var class="var">separation</var>))</code>.  Thus, if
one wants to make sure that <var class="var">to</var> is included in the list, one can
pass an expression of this exact type for <var class="var">to</var>.  Alternatively,
one can replace <var class="var">to</var> with a slightly larger value (or a slightly
more negative value if <var class="var">separation</var> is negative).
</p>
<p>Some examples:
</p>
<div class="example">
<pre class="example-preformatted">(number-sequence 4 9)
     ⇒ (4 5 6 7 8 9)
(number-sequence 9 4 -1)
     ⇒ (9 8 7 6 5 4)
(number-sequence 9 4 -2)
     ⇒ (9 7 5)
(number-sequence 8)
     ⇒ (8)
(number-sequence 8 5)
     ⇒ nil
(number-sequence 5 8 -1)
     ⇒ nil
(number-sequence 1.5 6 2)
     ⇒ (1.5 3.5 5.5)
</pre></div>
</dd></dl>

</div>
<div class="footnotes-segment">
<hr>
<h4 class="footnotes-heading">Footnotes</h4>

<h5 class="footnote-body-heading"><a id="FOOT5" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Lists.html#DOCF5">(5)</a></h5>
<p>There is no strictly equivalent way to add an element to
the end of a list.  You can use <code class="code">(append <var class="var">listname</var> (list
<var class="var">newelt</var>))</code>, which creates a whole new list by copying <var class="var">listname</var>
and adding <var class="var">newelt</var> to its end.  Or you can use <code class="code">(nconc
<var class="var">listname</var> (list <var class="var">newelt</var>))</code>, which modifies <var class="var">listname</var>
by following all the <small class="sc">CDR</small>s and then replacing the terminating
<code class="code">nil</code>.  Compare this to adding an element to the beginning of a
list with <code class="code">cons</code>, which neither copies nor modifies the list.</p>
</div>
<hr>
<div class="nav-panel">
<p>
Next: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Variables.html">Modifying List Variables</a>, Previous: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Elements.html">Accessing Elements of Lists</a>, Up: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Lists.html">Lists</a> &nbsp; [<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents" title="Table of contents" rel="contents">Contents</a>][<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Index.html" title="Index" rel="index">Index</a>]</p>
</div>





</body></html>