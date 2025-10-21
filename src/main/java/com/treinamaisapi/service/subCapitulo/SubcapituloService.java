package com.treinamaisapi.service.subCapitulo;

import com.treinamaisapi.common.dto.questao.request.SubcapituloRequest;
import com.treinamaisapi.common.dto.questao.response.SubcapituloResponse;
import com.treinamaisapi.entity.subCapitulo.Subcapitulo;
import com.treinamaisapi.repository.CapituloRepository;
import com.treinamaisapi.repository.SubCapituloRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import java.util.List;

@Service
@RequiredArgsConstructor
public class SubcapituloService {

    private final SubCapituloRepository subcapituloRepository;
    private final CapituloRepository capituloRepository;

    public SubcapituloResponse criar(SubcapituloRequest request) {
        var capitulo = capituloRepository.findById(request.getCapituloId())
                .orElseThrow(() -> new IllegalArgumentException("Capítulo não encontrado"));
        var subcapitulo = Subcapitulo.builder().nome(request.getNome()).capitulo(capitulo).build();
        subcapituloRepository.save(subcapitulo);
        return new SubcapituloResponse(subcapitulo.getId(), subcapitulo.getNome(),
                capitulo.getNome(), capitulo.getTema().getNome());
    }

    public List<SubcapituloResponse> listarPorCapitulo(Long capituloId) {
        return subcapituloRepository.findAll().stream()
                .filter(s -> s.getCapitulo().getId().equals(capituloId))
                .map(s -> new SubcapituloResponse(s.getId(), s.getNome(),
                        s.getCapitulo().getNome(), s.getCapitulo().getTema().getNome()))
                .toList();
    }
}

