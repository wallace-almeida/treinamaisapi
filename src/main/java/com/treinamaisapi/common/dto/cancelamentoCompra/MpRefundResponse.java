package com.treinamaisapi.common.dto.cancelamentoCompra;

import java.math.BigDecimal;

public record MpRefundResponse(String id, String status, BigDecimal amount) {}

