# SIqc

**Experimental Shiny application for laboratory quality-control data management.**

SIqc is an early-stage R/Shiny project exploring how laboratory QC activities could be organised around a structured database rather than managed only through spreadsheets or disconnected records.

The current prototype focuses on the **planning and management of QC activities**, with particular attention to repeatability studies and the storage and retrieval of QC information.

> **Status: experimental / work in progress.**
>
> This repository is not a production-ready laboratory information system. Several planned components are still incomplete, and the data model and workflow may change.

## What is currently explored

The project contains an R package structure generated with **golem** and a Shiny application layer.

Current code includes modules and functions for:

- planning QC activities;
- managing QC lists;
- repeatability studies;
- displaying repeatability results;
- SQL queries and database-oriented operations;
- application configuration and helpers.

The project uses `data.table` for data manipulation and a small set of Shiny/golem infrastructure packages.

## Design questions

The project was developed around practical questions that arise when QC information becomes a database-managed resource:

- How should QC activities and their requirements be represented?
- How can analytical results be linked to the corresponding QC plan?
- How should the database minimise unnecessary manual data entry?
- Which information should be derived automatically from the requirements of a QC procedure?
- How should concurrent users and database writes be handled?
- How can a development database be kept separate from production data?

These questions are part of the design exploration and should not be interpreted as a final specification.

## Project structure

The repository contains:

- `SIqc/` — the R package and Shiny application;
- `SIqc/R/` — application modules, database functions and helpers;
- `SIqc/data/` and `SIqc/data-raw/` — package data resources;
- `SIqc/tests/` — test infrastructure;
- `SIqc/renv.lock` — recorded R package environment.

The application follows a modular Shiny structure rather than concentrating all application logic in a single script.

## Development status

The project is intentionally marked **experimental**.

In particular, planned work includes:

- completing the workflow for entering and displaying proficiency-testing and fitness-for-purpose results;
- refining the database structure;
- evaluating how QC requirements can be retrieved or derived automatically;
- improving the handling of concurrent database writes, potentially using UUID-based identifiers;
- maintaining a separate database instance for development and testing.

The exact implementation is subject to change as the underlying workflow is clarified.

## Reproducibility

The project includes a `renv.lock` file to record the R package environment used during development.

The package declares dependencies including:

- `shiny`;
- `golem`;
- `config`;
- `data.table`;
- `testthat`.

Because SIqc is an unfinished application, successful execution may still depend on the current state of the development code and its configuration.

## Scope

SIqc is best considered a **software-development experiment at the intersection of laboratory quality management, database design and R/Shiny application development**.

It is not intended to replace a laboratory information management system (LIMS), a validated QC system, or documented laboratory procedures.

The repository is useful primarily for examining the design decisions involved in turning laboratory QC workflows into a reproducible, database-backed application.

## License

SIqc is released under the **GNU Affero General Public License v3 or later**. See the `LICENSE` file for details.