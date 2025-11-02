package com.treinamaisapi.common.dto.simulado.filtro;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TemaFiltroDTO {
    private Long id;
    private String nome;
    private List<CapituloFiltroDTO> capitulos;
}