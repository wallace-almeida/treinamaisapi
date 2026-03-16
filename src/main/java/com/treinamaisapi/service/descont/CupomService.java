package com.treinamaisapi.service.descont;


import com.treinamaisapi.common.dto.desconto.CriarCupomRequest;
import com.treinamaisapi.common.dto.desconto.CupomPreviewResponse;
import com.treinamaisapi.common.dto.desconto.CupomResponse;
import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.common.exception.NotFoundException;
import com.treinamaisapi.entity.desconto.CupomDesconto;
import com.treinamaisapi.entity.enums.desconto.TipoDesconto;
import com.treinamaisapi.entity.pacotes.Pacote;
import com.treinamaisapi.repository.CupomDescontoRepository;
import com.treinamaisapi.repository.PacoteRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;

@Service
@RequiredArgsConstructor
public class CupomService {

    private final CupomDescontoRepository cupomRepository;
    private final PacoteRepository pacoteRepository;

    public CupomPreviewResponse aplicarDesconto(Long usuarioId, Long pacoteId, String codigo) {
        Pacote pacote = pacoteRepository.findById(pacoteId)
                .orElseThrow(() -> new NotFoundException("Pacote não encontrado."));

        BigDecimal precoOriginal = pacote.getPreco().setScale(2, RoundingMode.HALF_UP);

        // sem cupom → retorna preview padrão
        if (codigo == null || codigo.isBlank()) {
            return CupomPreviewResponse.builder()
                    .valido(false)
                    .mensagem("Informe um cupom para aplicar.")
                    .precoOriginal(precoOriginal)
                    .desconto(BigDecimal.ZERO)
                    .precoFinal(precoOriginal)
                    .build();
        }

        CupomDesconto cupom = cupomRepository.findByCodigoIgnoreCase(codigo.trim())
                .orElseThrow(() -> new BusinessException("Cupom inválido."));

        validarRegrasBasicas(cupom, pacote, precoOriginal);

        BigDecimal desconto = calcularDesconto(cupom, precoOriginal);
        BigDecimal precoFinal = precoOriginal.subtract(desconto);

        if (precoFinal.compareTo(BigDecimal.ZERO) < 0) {
            precoFinal = BigDecimal.ZERO;
            desconto = precoOriginal;
        }

        precoFinal = precoFinal.setScale(2, RoundingMode.HALF_UP);
        desconto = desconto.setScale(2, RoundingMode.HALF_UP);

        return CupomPreviewResponse.builder()
                .valido(true)
                .mensagem("Cupom aplicado com sucesso.")
                .precoOriginal(precoOriginal)
                .desconto(desconto)
                .precoFinal(precoFinal)
                .build();
    }

    private void validarRegrasBasicas(CupomDesconto cupom, Pacote pacote, BigDecimal precoOriginal) {
        if (!cupom.isAtivo()) {
            throw new BusinessException("Cupom inativo.");
        }

        LocalDateTime agora = LocalDateTime.now();

        if (cupom.getLimiteUsosTotal() != null &&
                cupom.getUsosRealizados() >= cupom.getLimiteUsosTotal()) {
            throw new BusinessException("Cupom já atingiu o limite de utilizações.");
        }

        if (cupom.getInicioVigencia() != null && agora.isBefore(cupom.getInicioVigencia())) {
            throw new BusinessException("Cupom ainda não está vigente.");
        }

        if (cupom.getFimVigencia() != null && agora.isAfter(cupom.getFimVigencia())) {
            throw new BusinessException("Cupom expirado.");
        }

        if (cupom.getValorMinimoCompra() != null &&
                precoOriginal.compareTo(cupom.getValorMinimoCompra()) < 0) {
            throw new BusinessException("Cupom válido apenas para compras acima de R$ " + cupom.getValorMinimoCompra());
        }


    }

    private BigDecimal calcularDesconto(CupomDesconto cupom, BigDecimal precoOriginal) {
        if (cupom.getTipo() == TipoDesconto.PERCENTUAL) {
            BigDecimal percent = cupom.getValor()
                    .divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP);
            return precoOriginal.multiply(percent);
        }

        // VALOR_FIXO
        BigDecimal fixo = cupom.getValor().setScale(2, RoundingMode.HALF_UP);
        return fixo.min(precoOriginal);
    }

    public BigDecimal calcularPrecoFinal(Pacote pacote, String codigo) {
        BigDecimal precoOriginal = pacote.getPreco().setScale(2, RoundingMode.HALF_UP);

        if (codigo == null || codigo.isBlank()) {
            return precoOriginal;
        }

        CupomDesconto cupom = cupomRepository.findByCodigoIgnoreCase(codigo.trim())
                .orElseThrow(() -> new BusinessException("Cupom inválido."));

        validarRegrasBasicas(cupom, pacote, precoOriginal);

        BigDecimal desconto = calcularDesconto(cupom, precoOriginal);
        BigDecimal precoFinal = precoOriginal.subtract(desconto);

        if (precoFinal.compareTo(BigDecimal.ZERO) < 0) {
            precoFinal = BigDecimal.ZERO;
        }

        return precoFinal.setScale(2, RoundingMode.HALF_UP);
    }


    public CupomResponse criar(CriarCupomRequest req) {
        String codigo = normalizarCodigo(req.getCodigo());

        if (codigo == null || codigo.isBlank()) {
            throw new BusinessException("Código do cupom é obrigatório.");
        }
        if (req.getTipo() == null) {
            throw new BusinessException("Tipo de desconto é obrigatório.");
        }
        if (req.getValor() == null || req.getValor().compareTo(BigDecimal.ZERO) <= 0) {
            throw new BusinessException("Valor do cupom deve ser maior que zero.");
        }

        // regra: percentual não pode passar de 100
        if (req.getTipo() == TipoDesconto.PERCENTUAL && req.getValor().compareTo(new BigDecimal("100")) > 0) {
            throw new BusinessException("Percentual não pode ser maior que 100.");
        }

        // vigência coerente
        if (req.getInicioVigencia() != null && req.getFimVigencia() != null
                && req.getFimVigencia().isBefore(req.getInicioVigencia())) {
            throw new BusinessException("Fim de vigência não pode ser antes do início.");
        }

        // duplicidade
        if (cupomRepository.existsByCodigoIgnoreCase(codigo)) {
            throw new BusinessException("Já existe um cupom com esse código.");
        }

        CupomDesconto cupom = CupomDesconto.builder()
                .codigo(codigo)
                .tipo(req.getTipo())
                .valor(req.getValor().setScale(2, RoundingMode.HALF_UP))
                .ativo(req.isAtivo())
                .inicioVigencia(req.getInicioVigencia())
                .fimVigencia(req.getFimVigencia())
                .limiteUsosTotal(req.getLimiteUsosTotal())
                .limiteUsosPorUsuario(req.getLimiteUsosPorUsuario())
                .valorMinimoCompra(req.getValorMinimoCompra() != null
                        ? req.getValorMinimoCompra().setScale(2, RoundingMode.HALF_UP)
                        : null)
                .build();

        CupomDesconto salvo = cupomRepository.save(cupom);

        return CupomResponse.builder()
                .id(salvo.getId())
                .codigo(salvo.getCodigo())
                .tipo(salvo.getTipo())
                .valor(salvo.getValor())
                .ativo(salvo.isAtivo())
                .inicioVigencia(salvo.getInicioVigencia())
                .fimVigencia(salvo.getFimVigencia())
                .limiteUsosTotal(salvo.getLimiteUsosTotal())
                .limiteUsosPorUsuario(salvo.getLimiteUsosPorUsuario())
                .valorMinimoCompra(salvo.getValorMinimoCompra())
                .build();
    }

    private String normalizarCodigo(String codigo) {
        return codigo == null ? null : codigo.trim().toUpperCase();
    }
}
