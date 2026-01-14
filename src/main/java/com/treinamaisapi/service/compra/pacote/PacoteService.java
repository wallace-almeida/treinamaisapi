package com.treinamaisapi.service.compra.pacote;

import com.treinamaisapi.common.dto.concurso.response.ConcursoResponse;
import com.treinamaisapi.common.dto.pacote.request.PacoteRequest;
import com.treinamaisapi.common.dto.pacote.response.CatalogoPacoteDTO;
import com.treinamaisapi.common.dto.pacote.response.PacoteResponse;
import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.common.exception.NotFoundException;
import com.treinamaisapi.entity.Concurso;
import com.treinamaisapi.entity.enums.concursos.StatusConcurso;
import com.treinamaisapi.entity.pacotes.Pacote;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import com.treinamaisapi.entity.tema.Tema;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.*;
import com.treinamaisapi.service.compra.concurso.ConcursoService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PacoteService {

    private final PacoteRepository pacoteRepository;
    private final ConcursoRepository concursoRepository;
    private final ConcursoService concursoService;
    private final TemaRepository temaRepository;

    @Transactional
    public PacoteResponse criarPacote(PacoteRequest request) {
        Concurso concurso = concursoRepository.findById(request.getConcursoId())
                .orElseThrow(() -> new NotFoundException("Concurso não encontrado."));

        if (pacoteRepository.existsByNomeAndConcursoId(request.getNome(), concurso.getId())) {
            throw new BusinessException("Já existe um pacote com esse nome neste concurso.");
        }

        List<Tema> temas = validarTemas(request.getTemaIds());

        if (temas.isEmpty()) {
            throw new BusinessException("É obrigatório informar ao menos um tema.");
        }


        Pacote pacote = Pacote.builder()
                .nome(request.getNome())
                .descricao(request.getDescricao())
                .preco(request.getPreco())
                .duracaoDias(request.getDuracaoDias())
                .concurso(concurso)
                .temas(temas)
                .versao(1)
                .build();

        pacoteRepository.save(pacote);

        return PacoteResponse.builder()
                .id(pacote.getId())
                .nome(pacote.getNome())
                .descricao(pacote.getDescricao())
                .preco(pacote.getPreco())
                .duracaoDias(pacote.getDuracaoDias())
                .concursoNome(concurso.getNome())
                .temas(temas.stream().map(Tema::getNome).collect(Collectors.toList()))
                .versao(pacote.getVersao())
                .build();
    }

    private List<Tema> validarTemas(List<Long> temaIds) {
        List<Tema> temas = temaRepository.findAllById(temaIds);

        if (temas.size() != temaIds.size()) {
            throw new BusinessException("Um ou mais temas informados são inválidos.");
        }
        return temas;
    }

    @Transactional
    public PacoteResponse atualizarPacote(Long id, PacoteRequest request) {
        Pacote pacote = pacoteRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Pacote não encontrado"));

        if (request.getNome() != null &&
                pacoteRepository.existsByNomeAndConcursoId(request.getNome(), pacote.getConcurso().getId()) &&
                !request.getNome().equalsIgnoreCase(pacote.getNome())) {
            throw new BusinessException("Já existe outro pacote com esse nome neste concurso.");
        }

        // Atualiza somente se o valor for fornecido
        if (request.getNome() != null) {
            pacote.setNome(request.getNome());
        }
        if (request.getDescricao() != null) {
            pacote.setDescricao(request.getDescricao());
        }
        if (request.getPreco() != null) {
            pacote.setPreco(request.getPreco());
        }
        if (request.getDuracaoDias() != 0) { // se quiser considerar 0 como não enviado
            pacote.setDuracaoDias(request.getDuracaoDias());
        }
        if (request.getTemaIds() != null && !request.getTemaIds().isEmpty()) {
            List<Tema> temas = temaRepository.findAllById(request.getTemaIds());
            pacote.setTemas(temas);
        }

        // Incrementa versão a cada atualização
        pacote.setVersao(pacote.getVersao() + 1);

        pacoteRepository.save(pacote);

        return PacoteResponse.builder()
                .id(pacote.getId())
                .nome(pacote.getNome())
                .descricao(pacote.getDescricao())
                .preco(pacote.getPreco())
                .duracaoDias(pacote.getDuracaoDias())
                .concursoNome(pacote.getConcurso().getNome())
                .temas(pacote.getTemas().stream().map(Tema::getNome).collect(Collectors.toList()))
                .versao(pacote.getVersao())
                .build();
    }


    @Transactional(readOnly = true)
    public Integer buscarVersaoPorId(Long pacoteId) {
        Pacote pacote = pacoteRepository.findById(pacoteId)
                .orElseThrow(() -> new NotFoundException("Pacote não encontrado"));
        return pacote.getVersao();
    }

    @Transactional(readOnly = true)
    public List<Pacote> listarPacotesPorConcurso(Long concursoId) {

        Concurso concurso = concursoService.buscarEntidadePorId(concursoId);

        if (concurso.getStatus() != StatusConcurso.ATIVO) {
            throw new RuntimeException("Concurso não está ativo");
        }

        return pacoteRepository.findByConcursoIdAndAtivoTrue(concursoId);
    }

// pacotes para a tela de planos
@Transactional(readOnly = true)
public List<CatalogoPacoteDTO> listarCatalogo() {
    List<Pacote> pacotes = pacoteRepository.findByAtivoTrue(); // todos pacotes ativos

    return pacotes.stream()
            .map(p -> CatalogoPacoteDTO.builder()
                    .id(p.getId())
                    .nome(p.getNome())
                    .descricao(p.getDescricao())
                    .preco(p.getPreco())
                    .duracaoDias(p.getDuracaoDias())
                    .concursoNome(p.getConcurso().getNome())
                    .beneficios(p.getTemas().stream().map(Tema::getNome).toList())
                    .maisPopular(definirMaisPopular(p))
                    .build())
            .toList();
}

    private Boolean definirMaisPopular(Pacote pacote) {
        // Critério simples: pacotes acima de R$49,90 são mais populares (exemplo)
        return pacote.getPreco().compareTo(new BigDecimal("49.90")) >= 0;
    }


}
