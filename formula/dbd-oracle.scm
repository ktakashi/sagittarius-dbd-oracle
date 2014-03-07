(formula
 (description "DBD Oracle for Sagittarius")
 (version "HEAD")
 (homepage :url "https://github.com/ktakashi/sagittarius-dbd-oracle")
 (author :name "Takashi Kato" :email "ktakashi@ymail.com")
 (source 
  ;;:type tar :compression gzip
  :type zip
  :url "https://github.com/ktakashi/sagittarius-dbd-oracle/archive/master.zip")

 (install 
  (files "oracle.scm"
	 "dbd/oracle.scm"
	 "oracle/constant.scm"
	 "oracle/oci.scm")))