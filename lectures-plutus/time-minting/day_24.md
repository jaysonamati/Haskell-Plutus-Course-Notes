# Time

- tx has a "validity interval" (both endpoints optional) - time during which transaction can be valid.
- Plutus script only "See" the validity interval, NOT the current time.

- time measurement:
  - Node uses slots (at the moment: 1 slot = 1 second)
  - Plutus: real time (seconds since january 1st 1970 - Posixtime)
  
## Minting

