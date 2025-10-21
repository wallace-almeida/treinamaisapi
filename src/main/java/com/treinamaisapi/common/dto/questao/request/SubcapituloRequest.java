package com.treinamaisapi.common.dto.questao.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class SubcapituloRequest {
    @NotBlank
    private String nome;

    @NotNull
    private Long capituloId;
}
