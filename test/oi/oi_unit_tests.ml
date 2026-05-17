let () =
  Alcotest.run "oi"
    [
      Test_manifest_layer.suite;
      Test_manifest_build.suite;
      Test_manifest_registry.suite;
      Test_source_manifest.suite;
      Test_audit.suite;
      Test_provenance.suite;
      Test_keys.suite;
      Test_origin.suite;
      Test_makefile_export.suite;
      Test_depopt.suite;
      Test_depopt_matrix.suite;
    ]
