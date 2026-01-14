package com.treinamaisapi.common.dto.cancelamentoCompra;

import com.treinamaisapi.entity.enums.pacotes.StatusCompra;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@Builder
public class CancelamentoCompraResponse {
    private Long id;
    private StatusCompra status;
    private LocalDateTime dataCancelamento;
    private String mensagem;
}
