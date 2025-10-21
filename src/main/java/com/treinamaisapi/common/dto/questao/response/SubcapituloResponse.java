package com.treinamaisapi.common.dto.questao.response;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class SubcapituloResponse {
    private Long id;
    private String nome;
    private String capituloNome;
    private String temaNome;
}
