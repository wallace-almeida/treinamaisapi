package com.treinamaisapi.common.dto.simulado.filtro;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SubcapituloFiltroDTO {
    private Long id;
    private String nome;
}
