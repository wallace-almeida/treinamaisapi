package com.treinamaisapi.service.compra.pacote;

import com.treinamaisapi.common.dto.compra.response.CompraResponse;
import com.treinamaisapi.common.dto.compra.response.PacoteCompradoComUsuarioDTO;
import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.common.exception.NotFoundException;
import com.treinamaisapi.entity.enums.concursos.StatusConcurso;
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

@Service
@RequiredArgsConstructor
public class PacoteCompradoService {
    private final PacoteCompradoRepository pacoteCompradoRepository;
    private final UsuarioRepository usuarioRepository;
    private final PacoteRepository pacoteRepository;

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

        pacoteCompradoRepository
                .findByUsuarioIdAndPacoteIdAndStatus(
                        usuarioId, pacoteId, StatusCompra.APROVADA)
                .ifPresent(p -> {
                    if (!p.isExpirado()) {
                        throw new RuntimeException("Usuário já possui acesso ativo");
                    }
                });

        LocalDateTime agora = LocalDateTime.now();

        PacoteComprado compra = PacoteComprado.builder()
                .usuario(usuarioRepository.getReferenceById(usuarioId))
                .pacote(pacote)
                .dataCompra(agora)
                .dataExpiracao(agora.plusDays(pacote.getDuracaoDias()))
                .status(StatusCompra.APROVADA) // MVP
                .ativo(true)
                .build();

        return pacoteCompradoRepository.save(compra);
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

}
