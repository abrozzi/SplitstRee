read.breseq.gd <- function(file.gd) {

  types = c("SNP", "SUB", "DEL", "INS", "MOB", "AMP", "CON", "INV",
            "RA", "MC", "JC", "UN",
            "TSEQ", "PFLP", "RFLP", "PFGE", "PHYL", "CURA")

  fileOuts = c()

    for (ty in types) {

      fileOut = paste0(tempfile(), "_", ty)

      system(paste0("grep ^", ty, " ", file.gd, " > ", fileOut))

      fileOuts = c(fileOuts,fileOut)
  }

  RES = list()

  for ( file in fileOuts ) {

    type = unlist(strsplit(basename(file),"_"))[2]

    if(type=="SNP") {

      if(file.info(file)$size!=0) {

        SNP = data.frame(type=NA,
                         evidence_id=NA,
                         parent_ids=NA,
                         seq_id=NA,
                         position=NA,
                         new_seq=NA,
                         aa_new_seq=NA,
                         aa_position=NA,
                         aa_ref_seq=NA,
                         codon_new_seq=NA,
                         codon_number=NA,
                         codon_position=NA,
                         codon_ref_seq=NA,
                         gene_list=NA,
                         gene_name=NA,
                         gene_position=NA,
                         gene_product=NA,
                         gene_strand=NA,
                         html_gene_name=NA,
                         locus_tag=NA,
                         snp_type=NA,
                         transl_table=NA,
                         stringsAsFactors=FALSE
        )

        RL = readLines(file)

        totLines = length(RL)

        for ( i in 1 : totLines ) {

          cat(i,"\n")

          line = unlist(strsplit(RL[i],"\t"))

          field_values = sapply(strsplit(line,"="),function(s) s[2])

          newEntry = c(line[1:6], field_values[7:22])

          SNP = rbind(SNP, newEntry)
        }

        SNP = SNP[-1,]

        RES[[type]]=SNP

      } else {

        RES[[type]]=NULL
    }

    }

    if(type == "RA")

      RA = data.frame(type=NA,
                      evidence_id=NA,
                      parent_ids=NA,
                      seq_id=NA,
                      insert_position=NA,
                      ref_base=NA,
                      ref_base=NA
)

if(file.info(file)$size!=0){



}

  }

}
