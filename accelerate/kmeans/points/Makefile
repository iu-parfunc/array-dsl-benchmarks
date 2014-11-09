
CABAL=cabal
GEN_SAMPLES=./dist/build/gen-samples/gen-samples

CLUSTERS=5
MIN_POINTS=100000
MAX_POINTS=150000
SEED=1234

points.bin: $(GEN_SAMPLES)
	$(GEN_SAMPLES) $(CLUSTERS) $(MIN_POINTS) $(MAX_POINTS) $(SEED)

$(GEN_SAMPLES): cabal.sandbox.config gen-samples.cabal GenSamples.hs
	$(CABAL) install --only-dependencies
	$(CABAL) build

cabal.sandbox.config:
	$(CABAL) sandbox init --sandbox=../../.cabal-sandbox

clean:
	$(CABAL) clean
	$(RM) points.bin points clusters params

