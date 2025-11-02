package com.treinamaisapi.common.dto.pacote.request;

import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

@Data
public class PacoteRequest {
    private String nome;
    private String descricao;
    private BigDecimal preco;
    private int duracaoDias;
    private Long concursoId;
    private List<Long> temaIds; // IDs dos temas a associar
}