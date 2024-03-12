# Project Overview

This project is designed to help TORV LLC automate their reporting process. As you can see in the text of the proposal below, there are 5 main stages:

1.  Redeveloping the overall report layout

2.  Redesigning the data visualizations

3.  Redeveloping the report in RMarkdown

4.  Developing a workflow for importing and maintaining data

5.  Teaching Eric to generate reports

David will handle stages 1, 2, and 5 while Ellen will handle stages 3 and 4.

The deadline to complete this project is August 13.

# Reference Docs

There are a bunch of docs in the `reference-docs` folder. There are two sub-folders: 

- `torv-reports-etc` has two reports (the PDFs) that TORV has sent to clients. These are what we working to replicate (and improve). There is also an Excel file that is what the client currently uses to calculate values etc.
- `other-reports-etc` has a series of other reports done by other consultants. We can ignore these for now. 


# Proposal

## Overview

TORV LLC provides independent agronomic consulting, soil testing, water testing, and environmental testing for golf courses, sports turf, commercial and private landscapes, contractors, municipalities, and other green industries. With clients in Colorado, Montana, and New Mexico, CEO Eric Foerster brings his 25 years of experience in the field to his consulting work.

A central part of the work that TORV LLC does is producing reports for clients. These reports synthesize the results of soil, water, and geophysical tests. They are designed to help TORV clients take action to improve the quality of their facilities.

However, a problem that Eric is facing is that each report takes a significant amount of time to produce. Wrangling and cleaning data, calculating values, producing graphs, and combining everything into a single report takes many hours — time that could be spent working directly with current clients or connecting with new clients.

In addition, Eric feels that the current reports don't communicate as effectively as they could. Golf course superintendents, a main consumer of the reports, are busy and want concise summaries of the data. Without good data visualization, effective tables, and high-quality report layout, the messages that the reports are intended to communicate too often get lost.

Eric has reached out to David Keyes, head of R for the Rest of Us, for support in improving the process of generating reports and helping to ensure that the reports communicate more effectively than they currently do.

R for the Rest of Us provides both education and consulting services to current and aspiring R users. Much of our work focuses on helping organizations use data to communicate more effectively. Projects like this one, which combine data visualization, report layout, and workflow improvements, align well with our strengths. [**We have recently helped to design reports on housing and demographic data for policymakers in Connecticut**](https://rfortherestofus.com/success-stories/pschousing/), for example, a project similar in nature to this one. Coming up with strategies to use data to communicate effectively to non-data people while also doing so in a way that is efficient and reproducible is our core competency.

## Scope of Work

With the two main goals of this project — communicating more effectively and improving the workflow — there are several steps involved.

1.  **Redeveloping the overall report layout**. Before thinking about anything related to individual data visualizations, it is important to think about the design of the report as a whole. This will likely involve reordering sections of the report to bring the most important pieces forward, thinking about ways to use more visuals and less text, etc.

2.  **Redesigning the data visualizations**. There are currently several graphs, but their quality can be improved significantly. The most important change will be to highlight findings using titles, colors, etc. This will make it simpler for TORV clients to understand the findings and apply them to their work.

3.  **Redeveloping the report in RMarkdown** so that it can be generated at any point. RMarkdown is what makes it possible to auto-generate reports at any point. The RMarkdown document will combine narrative text, code to generate dynamic text (e.g. results from soil tests), and code to develop graphs and tables. Developing the report using this tool will ultimately make it possible to create a client report in seconds. And we will develop it in such a way that Eric can easily make changes or add client-specific recommendations to individual reports.

4.  **Developing a workflow for importing and maintaining data**. The current process to maintain data relies on significant manual work. Instead of copying data from CSVs to a single Excel spreadsheet, we will develop a workflow that imports new data automatically and maintains it in a single object within R, available to use at any point.

5.  **Teaching Eric to generate reports**. While the coding work will be done by R for the Rest of Us, the ultimate goal is to have Eric be able to generate reports on demand. The final step in this project, then, will involve teaching Eric about the code, how to generate reports, etc. This will be done in two Zoom sessions of up to 90 minutes each.

# Conventions

## File naming conventions

The lab report files must be named as follows in order to be read correctly by `update_database()`:

- [TestType]_[client-number]_[test-key]_[sample-type]_[date:MMDDYY].xlsx

For example:
- PHYSa_86259_S004_Physical_040422.xls
