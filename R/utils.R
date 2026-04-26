# Internal helpers shared across medication pipeline functions.
# These are NOT exported.

# Canonical MAR action codes indicating a medication was actively administered.
# Mirrors the default in clean_meds(); keep in sync when updating either.
.mar_given_codes <- function() {
     c(
          'anesthesia volume adjustment',
          'bolus from bag (dual sign required)',
          'bolus from bag',
          'continue to inpatient floor',
          'continued from or',
          'continued from pre',
          'given by other',
          'given during downtime',
          'given',
          'handoff (dual sign required)',
          'handoff',
          'new bag',
          'new bag/syringe/cartridge',
          'override pull',
          'rate change',
          'rate verify',
          'rate/dose change',
          'rate/dose changed',
          'rate/dose verify',
          'bolus',
          'restarted (dual sign required)',
          'restarted',
          'started during downtime',
          'started',
          'unheld by provider',
          'verification'
     )
}

# Canonical MAR action codes indicating a medication was stopped.
.mar_stopped_codes <- function() {
     c(
          'held',
          'held by provider',
          'mar hold',
          'stopped (dual sign required)',
          'stopped',
          'stop infusion'
     )
}
