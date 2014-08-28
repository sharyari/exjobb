#!/bin/bash
javac verifier/Parser/xmlParser/*.java
cd verifier/Parser
jar cfm parser.jar META-INF/MANIFEST.mf .
cp parser.jar ../..

