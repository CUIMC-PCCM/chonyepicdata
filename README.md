README
================

# Description

The purpose of this package is to provide a set of functions allowing
one to load, clean, and perform basic analyses on data from the
NewYork-Presbyterian (NYP) Epic environment. These methods are specific
to data generated from clinical care at the Morgan Stanley Children’s
Hospital of New York (aka “CHONY”) , but many functions should be
generalizable to other contexts within the NYP environment.

We are developing a standard format for how data are represented within
this system so that physicians, researchers, and quality specialists can
have a consistent way to load and process hospital data without having
to re-invent the wheel each time. This package is only useable with data
entered in the Epic EHR. Examples of data types are billing and
diagnosis codes codes, vital signs, laboratory values, and flowsheet
rows.
