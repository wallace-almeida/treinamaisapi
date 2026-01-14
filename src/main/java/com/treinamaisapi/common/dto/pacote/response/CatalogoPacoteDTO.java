package com.treinamaisapi.common.dto.pacote.response;

import lombok.Builder;
import lombok.Data;
import java.math.BigDecimal;
import java.util.List;

@Data
@Builder
public class CatalogoPacoteDTO {
    private Long id;
    private String nome;
    private String descricao;
    private BigDecimal preco;
    private Integer duracaoDias;
    private String concursoNome;
    private List<String> beneficios; // mapeado dos temas
    private Boolean maisPopular; // destaque para UI
}
