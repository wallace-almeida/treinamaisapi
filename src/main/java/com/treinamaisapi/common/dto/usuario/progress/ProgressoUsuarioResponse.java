package com.treinamaisapi.common.dto.usuario.progress;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ProgressoUsuarioResponse {

    private Long questoesResolvidas;
    private Double aproveitamento;
    private String tempoEstudo;
}
