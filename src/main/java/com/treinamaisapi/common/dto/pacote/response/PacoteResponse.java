package com.treinamaisapi.common.dto.pacote.response;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

@Data
@Builder
public class PacoteResponse {
    private Long id;
    private String nome;
    private String descricao;
    private BigDecimal preco;
    private int duracaoDias;
    private String concursoNome;
    private List<String> temas;
}
