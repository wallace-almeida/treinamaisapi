package com.treinamaisapi.service.compra.pacote;

import com.treinamaisapi.common.dto.compra.pix.gatewayPix.PixGateway;
import com.treinamaisapi.common.dto.compra.pix.response.CriarCompraPixResponse;
import com.treinamaisapi.common.dto.compra.pix.response.PixCobrancaResponse;
import com.treinamaisapi.common.dto.compra.response.CompraResponse;
import com.treinamaisapi.common.dto.compra.response.PacoteCompradoComUsuarioDTO;
import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.common.exception.NotFoundException;
import com.treinamaisapi.entity.enums.concursos.StatusConcurso;
import com.treinamaisapi.entity.enums.pacotes.MeioPagamento;
import com.treinamaisapi.entity.enums.pacotes.StatusCompra;
import com.treinamaisapi.entity.pacotes.Pacote;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.PacoteCompradoRepository;
import com.treinamaisapi.repository.PacoteRepository;
import com.treinamaisapi.repository.UsuarioRepository;
import org.springframework.transaction.annotation.Transactional;  // ✅ Spring
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class PacoteCompradoService {
    private final PacoteCompradoRepository pacoteCompradoRepository;
    private final UsuarioRepository usuarioRepository;
    private final PacoteRepository pacoteRepository;
    private final PixGateway pixGateway;


    @Transactional
    public CompraResponse comprar(Long usuarioId, Long pacoteId) {

        PacoteComprado compra = realizarCompra(usuarioId, pacoteId);

        return toResponse(compra);
    }

    private PacoteComprado realizarCompra(Long usuarioId, Long pacoteId) {

        Pacote pacote = pacoteRepository.findById(pacoteId)
                .orElseThrow(() -> new RuntimeException("Pacote não encontrado"));

        if (!pacote.isAtivo() ||
                pacote.getConcurso().getStatus() != StatusConcurso.ATIVO) {
            throw new RuntimeException("Pacote indisponível para compra");
        }

        LocalDateTime agora = LocalDateTime.now();

        Optional<PacoteComprado> compraExistenteOpt =
                pacoteCompradoRepository.findByUsuarioIdAndPacoteId(usuarioId, pacoteId);

        // ✅ CASO 1 — Já existe compra
        if (compraExistenteOpt.isPresent()) {
            PacoteComprado compra = compraExistenteOpt.get();

            // 🔒 Já está ativa
            if (compra.getStatus() == StatusCompra.APROVADA && !compra.isExpirado()) {
                throw new RuntimeException("Usuário já possui acesso ativo a este pacote");
            }

            // 🔁 Reativação (CANCELADA ou EXPIRADA)
            compra.setStatus(StatusCompra.APROVADA);
            compra.setAtivo(true);
            compra.setDataCompra(agora);
            compra.setDataExpiracao(agora.plusDays(pacote.getDuracaoDias()));
            compra.setDataCancelamento(null);
            compra.setMotivoCancelamento(null);

            return pacoteCompradoRepository.save(compra);
        }

        // ✅ CASO 2 — Primeira compra
        PacoteComprado novaCompra = PacoteComprado.builder()
                .usuario(usuarioRepository.getReferenceById(usuarioId))
                .pacote(pacote)
                .dataCompra(agora)
                .dataExpiracao(agora.plusDays(pacote.getDuracaoDias()))
                .status(StatusCompra.APROVADA)
                .ativo(true)
                .build();

        return pacoteCompradoRepository.save(novaCompra);
    }


    private CompraResponse toResponse(PacoteComprado compra) {
        return CompraResponse.builder()
                .id(compra.getId())
                .pacoteId(compra.getPacote().getId())
                .pacoteNome(compra.getPacote().getNome())
                .valor(compra.getPacote().getPreco())
                .status(compra.getStatus())
                .ativo(compra.isAtivo())
                .dataCompra(compra.getDataCompra())
                .dataExpiracao(compra.getDataExpiracao())
                .build();
    }


    @Transactional(readOnly = true)
    public List<PacoteCompradoComUsuarioDTO> listarComprasAtivas(Long usuarioId) {
        // Busca todos os pacotes ativos do usuário
        List<PacoteComprado> pacotesAtivos = pacoteCompradoRepository.findByUsuarioIdAndAtivoTrue(usuarioId);

        LocalDate hoje = LocalDate.now();

        // Converte cada PacoteComprado para DTO com cálculo de dias restantes
        return pacotesAtivos.stream()
                .map(pc -> {
                    LocalDate dataProva = pc.getPacote().getConcurso().getDataProva(); // vem da entidade Concurso
                    long diasRestantes = ChronoUnit.DAYS.between(hoje, dataProva);

                    return PacoteCompradoComUsuarioDTO.builder()
                            .compraId(pc.getId())
                            .pacoteId(pc.getPacote().getId())
                            .nomePacote(pc.getPacote().getNome())
                            .dataCompra(pc.getDataCompra())
                            .dataExpiracao(pc.getDataExpiracao())
                            .ativo(pc.isAtivo())
                            .usuarioId(pc.getUsuario().getId())
                            .nomeUsuario(pc.getUsuario().getNome())
                            .emailUsuario(pc.getUsuario().getEmail())
                            .concursoId(pc.getPacote().getConcurso().getId())
                            .nomeConcurso(pc.getPacote().getConcurso().getNome())
                            .dataDaProva(dataProva)
                            .diasRestantes(diasRestantes)
                            .build();
                })
                .toList();
    }


    @Transactional
    public void desativarPacoteExpirado(Long pacoteCompradoId) {
        PacoteComprado pacoteComprado = pacoteCompradoRepository.findById(pacoteCompradoId).orElseThrow(() -> new IllegalArgumentException("Pacote não encontrado."));

        if(pacoteComprado.isExpirado()) {
            pacoteComprado.setAtivo(false);
            pacoteCompradoRepository.save(pacoteComprado);
        }

    }


    // Pagamento por GATeway  meio Pix

    @Transactional
    public CriarCompraPixResponse criarCompraPix(Long usuarioId, Long pacoteId) {

        Pacote pacote = pacoteRepository.findById(pacoteId)
                .orElseThrow(() -> new BusinessException("Pacote não encontrado"));

        if (!pacote.isAtivo() ||
                pacote.getConcurso().getStatus() != StatusConcurso.ATIVO) {
            throw new BusinessException("Pacote indisponível para compra");
        }

        // impede duplicidade ativa
        pacoteCompradoRepository
                .findByUsuarioIdAndPacoteId(usuarioId, pacoteId)
                .ifPresent(compra -> {
                    if (compra.getStatus() == StatusCompra.APROVADA &&
                            !compra.isExpirado()) {
                        throw new BusinessException("Usuário já possui acesso ativo");
                    }
                });

        PacoteComprado compra = PacoteComprado.builder()
                .usuario(usuarioRepository.getReferenceById(usuarioId))
                .pacote(pacote)
                .status(StatusCompra.PENDENTE)
                .meioPagamento(MeioPagamento.PIX)
                .gateway("ASAAS")
                .ativo(false)
                .build();

        compra = pacoteCompradoRepository.save(compra);

        PixCobrancaResponse pix = pixGateway.criarCobranca(
                compra.getId(),
                pacote.getPreco()
        );

        compra.setPixTxId(pix.getTxId());
        compra.setPixExpiracao(pix.getExpiracao());

        pacoteCompradoRepository.save(compra);

        return CriarCompraPixResponse.builder()
                .compraId(compra.getId())
                .status(compra.getStatus())
                .qrCodeBase64(pix.getQrCodeBase64())
                .qrCodeCopiaCola(pix.getCopiaCola())
                .expiracaoPix(pix.getExpiracao())
                .build();
    }

    // Confirmar Pagamento

    @Transactional
    public void confirmarPagamentoPix(String txId) {

        PacoteComprado compra = pacoteCompradoRepository
                .findByPixTxId(txId)
                .orElseThrow(() -> new BusinessException("Compra não encontrada"));

        if (compra.getStatus() == StatusCompra.APROVADA) return;

        compra.setStatus(StatusCompra.APROVADA);
        compra.setAtivo(true);
        compra.setDataExpiracao(
                LocalDateTime.now().plusDays(
                        compra.getPacote().getDuracaoDias()
                )
        );

        pacoteCompradoRepository.save(compra);
    }








}
