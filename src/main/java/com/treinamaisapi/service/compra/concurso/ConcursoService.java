package com.treinamaisapi.service.compra.concurso;

import com.treinamaisapi.common.dto.concurso.request.ConcursoRequest;
import com.treinamaisapi.common.dto.concurso.response.ConcursoResponse;
import com.treinamaisapi.common.dto.concurso.response.ConcursoStatusResponse;
import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.common.exception.NotFoundException;
import com.treinamaisapi.entity.Concurso;
import com.treinamaisapi.entity.enums.concursos.StatusConcurso;
import com.treinamaisapi.entity.pacotes.Pacote;
import com.treinamaisapi.repository.ConcursoRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ConcursoService {

    private final ConcursoRepository concursoRepository;

    @Transactional
    public ConcursoResponse criarConcurso(ConcursoRequest request) {
        concursoRepository.findByNomeIgnoreCase(request.getNome()).ifPresent(c -> {
            throw new BusinessException( "Já existe um concurso com esse nome.");
        });

        Concurso concurso = Concurso.builder()
                .nome(request.getNome())
                .descricao(request.getDescricao())
                .dataProva(request.getDataProva())
                .build();

        concursoRepository.save(concurso);

        return ConcursoResponse.builder()
                .id(concurso.getId())
                .nome(concurso.getNome())
                .descricao(concurso.getDescricao())
                .dataProva(concurso.getDataProva())
                .pacotes(concurso.getPacotes() == null ? null :
                        concurso.getPacotes().stream()
                                .map(p -> p.getNome())
                                .collect(Collectors.toList()))
                .build();

    }

    @Transactional(readOnly = true)
    public Concurso buscarEntidadePorId(Long id) {
        return concursoRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Concurso não encontrado."));
    }


    @Transactional(readOnly = true)
    public ConcursoResponse buscarPorId(Long id) {
        Concurso concurso = buscarEntidadePorId(id);

        return ConcursoResponse.builder()
                .id(concurso.getId())
                .nome(concurso.getNome())
                .descricao(concurso.getDescricao())
                .dataProva(concurso.getDataProva())
                .pacotes(concurso.getPacotes().stream()
                        .map(Pacote::getNome)
                        .toList())
                .build();
    }



    public List<ConcursoResponse> listarAtivosResponse() {
        return concursoRepository.findByStatus(StatusConcurso.ATIVO)
                .stream()
                .map(c -> ConcursoResponse.builder()
                        .id(c.getId())
                        .nome(c.getNome())
                        .descricao(c.getDescricao())
                        .dataProva(c.getDataProva())
                        .build())
                .toList();
    }


    @Transactional
    public ConcursoStatusResponse ativarConcurso(Long concursoId) {
        Concurso concurso = buscarEntidadePorId(concursoId);

        if (concurso.getStatus() != StatusConcurso.FUTURO) {
            throw new BusinessException("Somente concursos FUTUROS podem ser ativados");
        }

        concurso.setStatus(StatusConcurso.ATIVO);

        return ConcursoStatusResponse.builder()
                .id(concurso.getId())
                .nome(concurso.getNome())
                .status(concurso.getStatus())
                .build();
    }


    @Transactional
    public ConcursoResponse encerrarConcurso(Long concursoId) {
        Concurso concurso = buscarEntidadePorId(concursoId);

        if (concurso.getStatus() != StatusConcurso.ATIVO) {
            throw new BusinessException("Somente concursos ATIVOS podem ser encerrados");
        }

        concurso.setStatus(StatusConcurso.ENCERRADO);

        return toResponse(concurso);
    }

    private ConcursoResponse toResponse(Concurso concurso) {
        return ConcursoResponse.builder()
                .id(concurso.getId())
                .nome(concurso.getNome())
                .descricao(concurso.getDescricao())
                .dataProva(concurso.getDataProva())
                .pacotes(
                        concurso.getPacotes() == null
                                ? List.of()
                                : concurso.getPacotes().stream()
                                .map(Pacote::getNome)
                                .toList()
                )
                .build();
    }



}
