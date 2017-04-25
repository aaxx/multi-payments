
Set of services and tools to accept multi-crypto payments during ICO.


Tools
-----

  - **`gen-addrs.sh`** − allows to generate one-time addresses and private
    keys for them.
    - generates csv files with columns: <currency code>; <address>; <private key>
    - to drop private keys run `sed 's/;[^;]*$//' *.csv`
    - supported cryptos
      - BTC
      - LTC
      - DASH
    - depends on `vanitygen` tool from https://github.com/exploitagency/vanitygen-plus
