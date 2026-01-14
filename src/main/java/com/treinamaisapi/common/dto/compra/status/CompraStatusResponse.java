package com.treinamaisapi.common.dto.compra.status;

import com.treinamaisapi.entity.enums.pacotes.StatusCompra;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class CompraStatusResponse {
    private Long compraId;
    private StatusCompra status;
}

