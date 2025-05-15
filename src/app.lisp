(in-package :256)

(defsection @index (:title "Index page")
  "
** Data collected and its importance.

*** IP address
We use this to link a user to their browsing behaviour. I suspect we can do the following:
- Locate people in the same household because of sharing a single IP, could couple this with geolocation.
- Follow a user across devices, fingerprint a user depending on their device usage patterns.
- Group VPN users, I don't know how that will be beneficial yet.

*** Access times
- Track the times a user uses the site, track by ip, cookie. This will be part of fingerprinting.

** Cookie tracking
I am not building for anyone outside of Uganda, so I won't concern myself with American and European regulations. We will track the user with cookies at will.")

(defroute index "/" ()
  (remote-addr*))
