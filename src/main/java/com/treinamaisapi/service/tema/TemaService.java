package com.treinamaisapi.service.tema;

import com.treinamaisapi.common.dto.questao.TemaRequest;
import com.treinamaisapi.common.dto.questao.TemaResponse;
import com.treinamaisapi.entity.tema.Tema;
import com.treinamaisapi.repository.TemaRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class TemaService {

    private final TemaRepository temaRepository;
    public TemaResponse criar (TemaRequest request) {
        var tema = Tema.builder().nome(request.nome()).build();
        temaRepository.save(tema);
        return new TemaResponse(tema.getNome(), tema.getId());
    }

    }


