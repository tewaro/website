---
title: Introduction to Developer Experience
author: tewaro
bibliography: ../bib/biblio.bib
bib-style: ieee.csl
link-citations: true
reference-section-title: Bibliography
tags: technical, teaching, epistomology, dx
---

= Intro
This discussion is about the study and understanding of developer experience in systems software.
Before we dive deep into the topic let's understand why we even care about developer experience.
Very inuitively, this is about getting people one level above our system to use our system. In order
to do this we need to spend some time actually understanding the world of software engineering. As
many of you have had internships, some of you maybe familiar with a few of these problems, however I
want to walk you through the day-to-day of a few different enginneers working in the field.
This is going to be a discussion of real people in my life with several of the details enhanced to
make the points significantly clearer. This way we keep their identities anonymous.

= Characters

First we have Atlantis, she's an engineer at Google working on the kernel team whose day to day is
to be responsible for the policies for energy saving. Then we have Billy, who serves on a team at
the heart of Cloudflare's serverless functions environment, Workers on the runtime of that system.
Finally we have, Charlie who works at Google as a site reliability engineer. These three engineers
inhabit the spectrum of Development versus Operations. A topic we will delve into further later.

== Day-to-Day

Atlantis spends her days chasing down bugs, developing new features, and designing policies.
This looks like spending a great deal of time designing abstractions and creating tests to ensure
that the expected behavior results in the correct and most efficient scheduling. What do the *users*
of that code look like? Who are these abstractions and new features for and how does that inform her
day to day workflow?

= Why Should You Care

Understanding what the developer experience for people using your system is like also helps you
comprehend what kind of abstractions you should design. If you want people to actually use your
system, you have to understand how they will experience that system. There is a lot of different
flow diagrams that describe what that process looks like and a ton of ways to break down this
process. But keep in mind very clearly at the end of the day in systems we are generally creating
our abstractions for some set of other developers. Those developers can be tempermental and as their
workflow changes so to must we re-examine our approaches.

= Key Design Principles

Many of you may have heard of the KISS principle Keep it Simple Stupid, this has a pretty
substantial corollary in systems design: ''Worse is Better''. This principle is so endemic to
software design that all the fun scrum stuff in the Agile method became such a meme.

Before design was done in the waterfall approach where each part of a new system is heavily
specified almost to an obsessive degree. This is often done for projects where the iteration cost is
expensive (think large real world engineering). This model was originally adopted by the government
for the first set of large software projects. However in most cases software is an evolving process.
So as you release a certain version of the product your users change their mind about what they
want, and realize that they were inaccurate when describing their requirements in the first place.

This is something that Atlantis, Billy, and Charlie have to contend with. The changing set of
requirements and new needs of users will inform what they prioritize day to day and week to week.

For the rest of this we are going to talk about 

== A brief aside about LLMs

LLMs are absolutely dominating the developer sphere and changing how everyone interacts with their
computer, however it actually re-inforces the 


= Intro
When we talk about system's software we are interested in first and foremost the correctness and
theoretical properties that the system intends to solve. However, correctness and theoretical
properties are downstream of a far more important conception: the Developer Experience.
In our world the actual users of our system are other developers and not the general public.
This conception is in my opinion the least studied part of Human Computer Interaction (HCI).
However, it is studied far unlike other forms of interaction because the user are a particular
sub-category obsessed with a different set of concerns. They all also start with a significantly
greater technical background. Before we dive any deeper into the world of developer experience, we
want to understand some characters who exist and have to think within this space. These are close
friends of mine and I'm going to give you a little window into their experience.

== Characters

First we have Atlantis, she's an engineer at who works on a kernel team whose day to day is to be
responsible for the policies in the kernel. Then we have Billy, who serves on a team at the heart of
Cloudflare's serverless functions environment, Workers on the runtime of that system. Finally we
have, Charlie who works at Google as a site reliability engineer. These three engineers inhabit the
spectrum of Development versus Operations. A topic we will delve into further later.

Actually Billy and Charlie have the exact same day to day job. The difference lies in the question
we just asked:

#table(
  columns: (auto, auto, auto, auto),
  inset: 10pt,
  align: horizon,
  table.header(
    [Character], [Who is it for?], [Deployment Environment], [Characterize Developer Experience]
  ),
  [Atlantis], [App Developers], [Phone and laptop devices], [limited testing environment and poor
  performance analysis tools],
  [Billy], [Other Developers and Internal Developers], [Cloudflare's Global Network], [Dedicated
  Build and Test frameworks along with a very immature performance testing framework],
  [Charlie], [Internal folks and other SREs], [Google's Global Network], [Dedicated Build, Test
  infrastructure, along with separate performance infra on a global scale]
)

Before we continue my job as a researcher should be (although academia in my field is quite cooked)
to improve the day-to-day of one of these folks or all of these folks in each paper.
So I have to carefully consider each of their developer experiences.


= The UNIX Philosophy
The core tenants are:
+ ''Make programs do one thing well. To do a new job, build afresh rather than complicate old programs
  by adding new features.''
+ ''Expect the output of every program to become the input to another, as yet unknown, program.
  Don't clutter output with extraneous information. Avoid stringently columnar or binary input
  formats.''
+ ''Design and build software, even operating systems , to be tried early, ideally within weeks.
  Don't hesitate to throw away the clumsy parts and rebuild them.''
+ ''Use tools in preference to unskilled help to lighten a programming task, even if you have to
  detour to build the tools and expect to throw away some of them after you've finished using them''

The philosophy summary: ''Write programs that do one thing and do it well. Write programs to work
together. Write programs to handle text streams because that is a universal interface.''

There are a LOT of different philosophies for other designs. We are specifically talking about UNIX
because this is an operating systems class.

= Development And Operations
Once you've actually developed your operating system, you have to go and somehow set it up so your
users can actually use this newly created thing. So how do we go about actually operating that
system.

== SLAs/SLOs
== CI & Testing
== HMD & CD
== On Call
=== Platform Abuse
= LLMs Change Nothing
= Why Companies Are the Way they Are

