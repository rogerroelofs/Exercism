module RnaTranscription exposing (toRNA)


toRNA : String -> Result String String
toRNA dna =
    case dna of
        "" ->
            Ok ""

        _ ->
            let
                rna =
                    String.map
                        (\nucleotide ->
                            case nucleotide of
                                'G' ->
                                    'C'

                                'C' ->
                                    'G'

                                'T' ->
                                    'A'

                                'A' ->
                                    'U'

                                _ ->
                                    'X'
                        )
                        dna
            in
            if String.contains "X" rna then
                Err "Invalid nucleotide found"
            else
                Ok rna