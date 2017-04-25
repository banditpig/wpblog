---
ID: 363
post_title: Text Classification 1.
author: BanditPig
post_date: 2017-04-25 14:21:54
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/04/25/text-classification-1/
published: true
---
[latexpage]
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />What we're trying to do is classify a piece of text, a review - it could be a review of film, a holiday, a car... anything. And we'll claim that a review can either be positive e.g. 'T<em>he bicycle was excellent and went very quickly uphill</em>' . Or a review can be negative. e.g. '<em>I was unhappy with the screwdriver, it was really poor and always turned the wrong way</em>'.

The task is to take an arbitrary piece of text - a review  - and decide which class it falls into - positive or negative. There are a number of mathematical techniques to do this and this post will look at Bayesian classification from a practical point of view.

Bayesian classifiers use Bayes theorem. Bayes theorem describes the probability of an event occurring given some prior knowledge of events that might be related to the event being considered.

So, imagine we have a large number of reviews that we <strong>know</strong> to be positive reviews and we also have a similar number of reviews <strong>known</strong> to be negative. Call these the training set.  How do we determine the class of a new unclassified review given the information that the training sets contain? Bayes theorem tells us that probability of a review being positive given the review (which is what we want to know) is the probability of the review given it is a positive review multiplied by the probability of it being positive divided by the probability of the review. That's a mouthful. So in symbols this becomes:

$p(positive|review) = \frac{p(review|positive)p(positive)}{p(review)}$

and there is the corresponding formula for a review being negative. i.e.

$p(negative|review) = \frac{p(review|negative)p(negative)}{p(review)}$

and whichever of the above has the largest value will be the class that the review falls into - positive or negative.  The formulae can be simplified, ${p(review)}$ is constant and can be ignored. Also as we have two outcome classes, positive or negative, then

$p(positive)=p(negative)=0.5$

This too is constant so could be ignored. So that now leaves the calculation of

$p(review|positive)$ and $p(review|negative)$

It is perhaps possible that a review to be classified already has an exact copy in the training set however this is unlikely so we assume not and break the review into a list of words and take each word in turn and obtain the probability of the word being positive and multiply the probabilities together. i.e.

$p(review|positive)=p(w_1|positive)p(w_2|positive)p(w_3|positive)...p(w_n|positive)$

where $p(w_1|positive)$ is the probability of the first word being positive and similarly for the other words. This now leaves the calculation of a given word being positive or negative. Well this is where the training sets are used and for a given word $w_n$ we count how many times it occurs in all of the positive reviews in the training set and divide it by the total number of words in the positive reviews training set. At this point the mechanism becomes one of doing a 'one-off' processing of the training sets to get the frequency of the words and then turning the handle to classify arbitrary input reviews.

All of this is a very simple discussion of the technique and doesn't consider for example what happens if a word doesn't exist in the training data. It also ignores the possibility of errors due to repeated multiplication of small decimal numbers and so on. These and other details can be seen in this discussion of <a href="https://en.wikipedia.org/wiki/Naive_Bayes_spam_filtering#Other_expression_of_the_formula_for_combining_individual_probabilities">Bayes Spam Filter</a>.

However what we have here  is a starting point for Bayesian classification and the next post will give some example code using Python and the <a href="http://www.nltk.org/">NLTK library</a>.

Thanks for reading...