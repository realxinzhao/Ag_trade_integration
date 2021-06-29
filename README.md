# Ag_trade_integration
An R project for generating Ag market integration scenarios (agtrade.xml) in GCAM 

This project generates agtrade.xml outside gcamdata for more flexible specifications of future trade scenarios. It requires trade-related input data from gcamdata. The produced agtrade.xml with different integration scenarios can be added in GCAM configuration files.

Note that the files in the input folder came from gcamdata, either gcamdata input files or queried output files. GCAM query files are provided in the folder GCAMqueryfile for generating initial trade data from a GCAM reference database. Those data are used for calculating initial share weights used in the logit-based Armington approach.
Two future trade scenarios are specified in agtrade_xml.R, i.e., integrate.sw.rate = 1.08 and integrate.sw.rate = 1.22, to generate two integration scenarios, e.g., ag_trade_sw_default.xml and ag_trade_sw_high.xml.

Note that the integration scenarios were developed base on a bilateral Armignotn trade framework. So by default, regions (e.g., importer or exporter) were not distinguished in the parameter estimation process. That is, when applying the scenarios to the gross trade modeling framework in GCAM, there may be inconsistency and leading to high gloss trade. However, the net trade and supply responses should behave reasonably. Depending on the study purpose, more specific (e.g., regional) trade scenarios can be developed.
