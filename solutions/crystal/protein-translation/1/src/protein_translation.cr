module ProteinTranslation
  def self.protein_tx 
    {
      "AUG" => "Methionine",
      "UUU" => "Phenylalanine",
      "UUC" => "Phenylalanine",
      "UUA" => "Leucine",
      "UUG" => "Leucine",
      "UCU" => "Serine",
      "UCC" => "Serine",
      "UCA" => "Serine",
      "UCG" => "Serine",
      "UAU" => "Tyrosine",
      "UAC" => "Tyrosine",
      "UGU" => "Cysteine",
      "UGC" => "Cysteine",
      "UGG" => "Tryptophan",
      "UAA" => "STOP",
      "UAG" => "STOP",
      "UGA" => "STOP",
    }
  end

  def self.proteins(strand : String) : Array(String)
    strand
      .scan(/.{1,3}/)
      .each
      .map { |codon| protein(codon[0]) }
      .take_while { |protein| protein != "STOP" }
      .to_a
  end

  private def self.protein(codon : String) : String
    self.protein_tx.fetch(codon) { raise ArgumentError.new }
  end
end
