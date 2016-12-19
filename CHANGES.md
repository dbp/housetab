## Housetab - revamp

The current version of Housetab was created in mid-2011, and since
software moves pretty quickly, a lot of how it works under the hood is
pretty out of date. Since it works pretty well, in some ways this
isn't a big deal, but it also means that it is more of a hassle to
maintain than it could be if it was using more modern tools. Since
it's a totally voluntary project (which makes negative money, since it
costs money to host), this isn't ideal, and is the motivation for
rewriting it. It should come with some improvements, but it also
involves removing some features, as we're creating the whole thing
from scratch and only have so much time to dedicate to it!

## What will improve

1. Mobile. Housetab should now work much better on mobile. Part of this is that
we're doing away with a lot of the design, as the current design is
extremely un-mobile friendly and we don't have the time to do a redesign
that is!

2. Multiple email addresses, no more passwords. In order to
streamline, we're getting rid of passwords. Instead, you will enter
the account name and then select one of the email addresses associated
and we'll send you a link that when you click will log you in. The
logins should last for a while (weeks at least), and there won't be
any more passwords. Since we previously supported only a single
email, you'll have to start out just logging in with that, and then
once you are logged in you can add more email addresses.

3. Data export. One of the features that we wanted to add to the
existing site was an easy way to export all of your data, but changing
the current site is currently quite hard. You'll now be able to easily
export all your data as a CSV that you can either use as a backup or
process with a spreadsheet tool.

## What will stay the same

The core functionality - adding people, entering transactions, and seeing
what the balances are - are all going to remain the same.

## What will go away

The entire history functionality is going to go away. There was a lot
of work put into it, and realistically it just didn't do much. If you need an
audit log of data, you probably shouldn't be using a system that doesn't
have separate accounts for separate people! And if you are worried about
deleting data, the improvement above that lets you download your data
should let you do what you need.

The tutorial is also going away. It's a lot of work to implement, and
unfortunately we don't have time to re-create it. For current users,
this is probably irrelevant, as you already know how to use it!
