package com.treinamaisapi.common.dto.desconto;

import lombok.Data;

@Data
public class CupomPreviewRequest {
    private String codigo;
    private Long pacoteId;
}
