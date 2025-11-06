package com.treinamaisapi.service.compra.pacote;

import com.treinamaisapi.common.dto.pacote.request.PacoteRequest;
import com.treinamaisapi.common.dto.pacote.response.PacoteResponse;
import com.treinamaisapi.entity.Concurso;
import com.treinamaisapi.entity.pacotes.Pacote;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import com.treinamaisapi.entity.tema.Tema;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PacoteService {

    private final PacoteRepository pacoteRepository;
    private final ConcursoRepository concursoRepository;
    private final TemaRepository temaRepository;

    @Transactional
    public PacoteResponse criarPacote(PacoteRequest request) {
        Concurso concurso = concursoRepository.findById(request.getConcursoId())
                .orElseThrow(() -> new IllegalArgumentException("Concurso não encontrado."));

        if (pacoteRepository.existsByNomeAndConcursoId(request.getNome(), concurso.getId())) {
            throw new IllegalStateException("Já existe um pacote com esse nome neste concurso.");
        }

        List<Tema> temas = temaRepository.findAllById(request.getTemaIds());
        if (temas.isEmpty()) {
            throw new IllegalArgumentException("Nenhum tema válido foi informado.");
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

    @Transactional
    public PacoteResponse atualizarPacote(Long id, PacoteRequest request) {
        Pacote pacote = pacoteRepository.findById(id)
                .orElseThrow(() -> new RuntimeException("Pacote não encontrado"));

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
                .orElseThrow(() -> new RuntimeException("Pacote não encontrado"));
        return pacote.getVersao();
    }

}
